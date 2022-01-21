package zio.json.internal

import scala.annotation._
import zio.json.Decoder.{ JsonError, UnsafeJson }
import scala.util.Properties.propOrNone

// tries to stick to the spec, but maybe a bit loose in places (e.g. numbers)
//
// https://www.json.org/json-en.html
object Lexer {
  val NumberMaxBits: Int = propOrNone("zio.json.number.bits").getOrElse("128").toInt

  // True if we got a string (implies a retraction), False for }
  def firstObject(trace: List[JsonError], in: RetractReader): Boolean =
    (in.nextNonWhitespace(): @switch) match {
      case '"' =>
        in.retract()
        true
      case '}' => false
      case c =>
        throw UnsafeJson(
          JsonError.Message(s"expected string or '}' got '$c'") :: trace
        )
    }

  // True if we got a comma, and False for }
  def nextObject(trace: List[JsonError], in: OneCharReader): Boolean =
    (in.nextNonWhitespace(): @switch) match {
      case ',' => true
      case '}' => false
      case c =>
        throw UnsafeJson(
          JsonError.Message(s"expected ',' or '}' got '$c'") :: trace
        )
    }

  // True if we got anything besides a ], False for ]
  def firstArray(trace: List[JsonError], in: RetractReader): Boolean =
    (in.nextNonWhitespace(): @switch) match {
      case ']' => false
      case _ =>
        in.retract()
        true
    }
  def nextArray(trace: List[JsonError], in: OneCharReader): Boolean =
    (in.nextNonWhitespace(): @switch) match {
      case ',' => true
      case ']' => false
      case c =>
        throw UnsafeJson(
          JsonError.Message(s"expected ',' or ']' got '$c'") :: trace
        )
    }

  // avoids allocating lots of strings (they are often the bulk of incoming
  // messages) by only checking for what we expect to see (Jon Pretty's idea).
  //
  // returns the index of the matched field, or -1
  def field(
    trace: List[JsonError],
    in: OneCharReader,
    matrix: StringMatrix
  ): Int = {
    val f = ordinal(trace, in, matrix)
    char(trace, in, ':')
    f
  }

  def ordinal(
    trace: List[JsonError],
    in: OneCharReader,
    matrix: StringMatrix
  ): Int = {
    val stream = streamingString(trace, in)

    var i: Int   = 0
    var bs: Long = matrix.initial
    var c: Int   = -1
    while ({ c = stream.read(); c != -1 }) {
      bs = matrix.update(bs, i, c)
      i += 1
    }
    bs = matrix.exact(bs, i)
    matrix.first(bs)
  }

  private[this] val ull: Array[Char]  = "ull".toCharArray
  private[this] val alse: Array[Char] = "alse".toCharArray
  private[this] val rue: Array[Char]  = "rue".toCharArray

  // if out is non-null then the JSON is written as it is read, up to
  // normalisation. If an exception is thrown, the contents of the Writer are
  // undefined.
  def skipValue(trace: List[JsonError], in: RetractReader, out: java.io.Writer): Unit = {
    val next = in.nextNonWhitespace()
    if (out ne null) out.append(next)
    (next: @switch) match {
      case 'n' =>
        readChars(trace, in, ull, "null")
        if (out ne null) out.write(ull)
      case 'f' =>
        readChars(trace, in, alse, "false")
        if (out ne null) out.write(alse)
      case 't' =>
        readChars(trace, in, rue, "true")
        if (out ne null) out.write(rue)
      case '{' =>
        var first = true
        if (firstObject(trace, in)) {
          while ({
            if (first) first = false
            else if (out ne null) out.write(',')
            char(trace, in, '"')
            if (out ne null) out.write('"')
            skipString(trace, in, out)
            char(trace, in, ':')
            if (out ne null) out.write(':')
            skipValue(trace, in, out)
            nextObject(trace, in)
          }) {}
        }
        if (out ne null) out.append('}')
      case '[' =>
        var first = true
        if (firstArray(trace, in)) {
          while ({
            if (first) first = false
            else if (out ne null) out.write(',')
            skipValue(trace, in, out)
            nextArray(trace, in)
          }) {}
        }
        if (out ne null) out.append(']')
      case '"' =>
        skipString(trace, in, out)
      case '-' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
        skipNumber(trace, in, out)
      case c => throw UnsafeJson(JsonError.Message(s"unexpected '$c'") :: trace)
    }
  }

  def skipNumber(trace: List[JsonError], in: RetractReader, out: java.io.Writer): Unit = {
    var c: Char = ' '
    try while ({ c = in.readChar(); isNumber(c) }) {
      if (out ne null) out.append(c)
    } catch {
      case UnexpectedEnd => // top level number
    }
    in.retract()
  }

  def skipString(trace: List[JsonError], in: OneCharReader, out: java.io.Writer): Unit = {
    var c: Char = ' '
    var escaped = false
    var end     = false

    while (!end) {
      c = in.readChar()
      if (out ne null) out.append(c)

      if (!escaped && c == '"') end = true
      else if (escaped) escaped = false
      else if (c == '\\') escaped = true
    }
  }

  // useful for embedded documents, e.g. CSV contained inside JSON
  def streamingString(
    trace: List[JsonError],
    in: OneCharReader
  ): OneCharReader = {
    char(trace, in, '"')
    new EscapedString(trace, in)
  }

  def string(trace: List[JsonError], in: OneCharReader): CharSequence = {
    char(trace, in, '"')
    val stream = new EscapedString(trace, in)

    val sb = new FastStringWriter(64)
    while (true) {
      val c = stream.read()
      if (c == -1)
        return sb.buffer // mutable thing escapes, but cannot be changed
      sb.append(c.toChar)
    }
    throw UnsafeJson(JsonError.Message("impossible string") :: trace)
  }

  def boolean(trace: List[JsonError], in: OneCharReader): Boolean =
    (in.nextNonWhitespace(): @switch) match {
      case 't' =>
        readChars(trace, in, rue, "true")
        true
      case 'f' =>
        readChars(trace, in, alse, "false")
        false
      case c =>
        throw UnsafeJson(
          JsonError.Message(s"expected 'true' or 'false' got $c") :: trace
        )
    }

  def byte(trace: List[JsonError], in: RetractReader): Byte = {
    checkNumber(trace, in)
    try {
      val i = UnsafeNumbers.byte(in)
      in.retract()
      i
    } catch {
      case UnsafeNumbers.UnsafeNumber =>
        throw UnsafeJson(JsonError.Message("expected a Byte") :: trace)
    }
  }

  def short(trace: List[JsonError], in: RetractReader): Short = {
    checkNumber(trace, in)
    try {
      val i = UnsafeNumbers.short(in)
      in.retract()
      i
    } catch {
      case UnsafeNumbers.UnsafeNumber =>
        throw UnsafeJson(JsonError.Message("expected a Short") :: trace)
    }
  }

  def int(trace: List[JsonError], in: RetractReader): Int = {
    checkNumber(trace, in)
    try {
      val i = UnsafeNumbers.int(in)
      in.retract()
      i
    } catch {
      case UnsafeNumbers.UnsafeNumber =>
        throw UnsafeJson(JsonError.Message("expected an Int") :: trace)
    }
  }

  def long(trace: List[JsonError], in: RetractReader): Long = {
    checkNumber(trace, in)
    try {
      val i = UnsafeNumbers.long(in)
      in.retract()
      i
    } catch {
      case UnsafeNumbers.UnsafeNumber =>
        throw UnsafeJson(JsonError.Message("expected a Long") :: trace)
    }
  }

  def biginteger(
    trace: List[JsonError],
    in: RetractReader
  ): java.math.BigInteger = {
    checkNumber(trace, in)
    try {
      val i = UnsafeNumbers.biginteger(in, NumberMaxBits)
      in.retract()
      i
    } catch {
      case UnsafeNumbers.UnsafeNumber =>
        throw UnsafeJson(JsonError.Message(s"expected a $NumberMaxBits bit BigInteger") :: trace)
    }
  }

  def float(trace: List[JsonError], in: RetractReader): Float = {
    checkNumber(trace, in)
    try {
      val i = UnsafeNumbers.float(in, NumberMaxBits)
      in.retract()
      i
    } catch {
      case UnsafeNumbers.UnsafeNumber =>
        throw UnsafeJson(JsonError.Message("expected a Float") :: trace)
    }
  }

  def double(trace: List[JsonError], in: RetractReader): Double = {
    checkNumber(trace, in)
    try {
      val i = UnsafeNumbers.double(in, NumberMaxBits)
      in.retract()
      i
    } catch {
      case UnsafeNumbers.UnsafeNumber =>
        throw UnsafeJson(JsonError.Message("expected a Double") :: trace)
    }
  }

  def bigdecimal(
    trace: List[JsonError],
    in: RetractReader
  ): java.math.BigDecimal = {
    checkNumber(trace, in)
    try {
      val i = UnsafeNumbers.bigdecimal(in, NumberMaxBits)
      in.retract()
      i
    } catch {
      case UnsafeNumbers.UnsafeNumber =>
        throw UnsafeJson(JsonError.Message(s"expected a $NumberMaxBits BigDecimal") :: trace)
    }
  }

  // really just a way to consume the whitespace
  private def checkNumber(trace: List[JsonError], in: RetractReader): Unit = {
    (in.nextNonWhitespace(): @switch) match {
      case '-' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => ()
      case c =>
        throw UnsafeJson(
          JsonError.Message(s"expected a number, got $c") :: trace
        )
    }
    in.retract()
  }

  // optional whitespace and then an expected character
  @inline def char(trace: List[JsonError], in: OneCharReader, c: Char): Unit = {
    val got = in.nextNonWhitespace()
    if (got != c)
      throw UnsafeJson(JsonError.Message(s"expected '$c' got '$got'") :: trace)
  }

  @inline def charOnly(
    trace: List[JsonError],
    in: OneCharReader,
    c: Char
  ): Unit = {
    val got = in.readChar()
    if (got != c)
      throw UnsafeJson(JsonError.Message(s"expected '$c' got '$got'") :: trace)
  }

  // non-positional for performance
  @inline private[this] def isNumber(c: Char): Boolean =
    (c: @switch) match {
      case '+' | '-' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '.' | 'e' | 'E' =>
        true
      case _ => false
    }

  def readChars(
    trace: List[JsonError],
    in: OneCharReader,
    expect: Array[Char],
    errMsg: String
  ): Unit = {
    var i: Int = 0
    while (i < expect.length) {
      if (in.readChar() != expect(i))
        throw UnsafeJson(JsonError.Message(s"expected '$errMsg'") :: trace)
      i += 1
    }
  }

}

// A Reader for the contents of a string, taking care of the escaping.
//
// `read` can throw extra exceptions on badly formed input.
final class EscapedString(trace: List[JsonError], in: OneCharReader) extends OneCharReader {

  private[this] var escaped = false

  override def read(): Int = {
    val c = in.readChar()
    if (escaped) {
      escaped = false
      (c: @switch) match {
        case '"' | '\\' | '/' | 'b' | 'f' | 'n' | 'r' | 't' => c
        case 'u'                                            => nextHex4()
        case _ =>
          throw UnsafeJson(
            JsonError.Message(s"invalid '\\${c.toChar}' in string") :: trace
          )
      }
    } else if (c == '\\') {
      escaped = true
      read()
    } else if (c == '"') -1 // this is the EOS for the caller
    else if (c < ' ')
      throw UnsafeJson(JsonError.Message("invalid control in string") :: trace)
    else c
  }

  // callers expect to get an EOB so this is rare
  def readChar(): Char = {
    val v = read()
    if (v == -1) throw UnexpectedEnd
    v.toChar
  }

  // consumes 4 hex characters after current
  def nextHex4(): Int = {
    var i: Int     = 0
    var accum: Int = 0
    while (i < 4) {
      var c: Int = in.read()
      if (c == -1)
        throw UnsafeJson(JsonError.Message("unexpected EOB in string") :: trace)
      c =
        if ('0' <= c && c <= '9') c - '0'
        else if ('A' <= c && c <= 'F') c - 'A' + 10
        else if ('a' <= c && c <= 'f') c - 'a' + 10
        else
          throw UnsafeJson(
            JsonError.Message("invalid charcode in string") :: trace
          )
      accum = accum * 16 + c
      i += 1
    }
    accum
  }

}
