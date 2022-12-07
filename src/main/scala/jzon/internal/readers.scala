package jzon.internal

import java.util.Arrays

import scala.annotation.switch
import scala.util.control.NoStackTrace

trait OneCharReader {
  // Cautious read: -1 on EOF
  def read(): Int

  // Optimistic read: throws UnexpectedEnd on EOF
  def readChar(): Char

  // should probably have a limit here to protect against attack vectors
  // consisting of an infinite stream of space.
  //
  // whitespace is defined in the json spec and may differ from Java
  def nextNonWhitespace(): Char = {
    var c = readChar()
    while (isWhitespace(c)) {
      c = readChar()
    }
    c
  }

  // profiled to be faster than Character.isWhitespace
  @inline protected final def isWhitespace(c: Char): Boolean =
    (c: @switch) match {
      case ' '  => true
      case '\r' => true
      case '\n' => true
      case '\t' => true
      case _    => false
    }
}

object UnexpectedEnd extends Exception("if you see this a dev made a mistake") with NoStackTrace

/**
 * A Reader that can retract and replay the last char that it read.
 *
 * This is essential when parsing contents that do not have a terminator
 * character, e.g. numbers, whilst preserving the non-significant character for
 * further processing.
 */
trait RetractReader extends OneCharReader {

  /** Behaviour is undefined if called more than once without a read()
      Behaviour is also undefined if called after reading a surrogate. */
  def retract(): Unit
}

final class FastCharSequence(s: Array[Char]) extends CharSequence {
  def length: Int          = s.length
  def charAt(i: Int): Char = s(i)
  def subSequence(start: Int, end: Int): CharSequence =
    new FastCharSequence(Arrays.copyOfRange(s, start, end))
}

// java.io.StringReader uses a lock, which reduces perf by x2, this also allows
// fast retraction and access to raw char arrays (which are faster than Strings)
final class FastStringReader(cs: CharSequence) extends RetractReader {
  private[this] var i: Int = 0

  private[this] def read_(): Char = {
    val c = cs.charAt(i)
    i += 1
    c
  }

  @inline def eof(): Boolean = i >= cs.length
  // consumed means that somebody read all the way up to the EOF
  @inline def consumed(): Boolean = i > cs.length

  override def read(): Int = {
    if (eof()) {
      i += 1
      return -1
    }

    read_()
  }

  override def readChar(): Char = {
    if (eof()) {
      i += 1
      throw UnexpectedEnd
    }

    read_()
  }

  override def retract(): Unit = {
    if (i <= 0) throw new IllegalStateException("if you see this a dev made a mistake")

    i -= 1
  }
}

final class FastBytesReader(bytes: ByteInput) extends RetractReader {
  private[this] var retracted: Boolean = false
  private[this] var last: Int          = -1

  // if surrogate is non-zero it holds the lower surrogate of a UTF-16 that must
  // be returned by the next read instead of querying the array. See
  // https://en.wikipedia.org/wiki/UTF-16
  private[this] var lowSurrogate: Int = 0

  @inline def eof(): Boolean = !retracted && lowSurrogate == 0 && bytes.eof()

  @inline private[this] def readByte(): Int = {
    val b = bytes.read()
    if (b == -1) throw UnexpectedEnd
    b
  }

  private[this] def read_(): Char = {
    if (retracted) {
      retracted = false
      return last.toChar
    }
    if (lowSurrogate > 0) {
      val c = lowSurrogate.toChar
      lowSurrogate = 0
      return c
    }
    val a = readByte()

    // https://en.wikipedia.org/wiki/UTF-8
    // 0x3F = 0011 1111
    // 0x1F = 0001 1111
    // 0x0F = 0000 1111
    // 0x07 = 0000 0111

    // according to the unicode standard, all the higher bytes should start with
    // 0x10, we should really check that.

    ((a >> 4): @switch) match {
      // 0xxxxxxx
      case 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 =>
        a.toChar

      // 1100xxxx 1101xxxx
      case 12 | 13 =>
        val b = readByte()

        (((a & 0x1F) << 6) | (b & 0x3F)).toChar

      // 1110xxxx
      case 14 =>
        val b = readByte()
        val c = readByte()

        (((a & 0x0F) << 12) | ((b & 0x3F) << 6) | (c & 0x3F)).toChar

      // 1111xxxx
      case 15 =>
        val b = readByte()
        val c = readByte()
        val d = readByte()

        val codepoint = ((a & 0x07) << 18) | ((b & 0x3F) << 12) | (c & 0x3F) << 6 | (d & 0x3F)
        lowSurrogate = Character.lowSurrogate(codepoint)
        Character.highSurrogate(codepoint)

      case _ =>
        throw UnexpectedEnd
    }
  }

  override def read(): Int = {
    if (eof()) {
      last = -1
      return -1
    }
    val c = read_()
    last = c
    c
  }

  override def readChar(): Char = {
    val c = read_()
    last = c
    c
  }

  override def retract(): Unit =
    retracted = true
}
