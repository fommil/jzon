package jzon.async

import java.io.Closeable

import scala.annotation._
import scala.util.control.NoStackTrace

import jzon.internal._

// Consumes an infinite stream of byte arrays containing multiple JSON object
// and array payloads (but not primitive values) that may be separated by
// whitespace, sending each payload to a sink without allocating memory [1].
//
// Throws exceptions at the producer indefinitely if an invalid payload is
// encountered or if a payload is larger than `maxLength`. An empty `Chunks`
// will be sent to the sink if there is an error or if the caller is finished,
// which may be signalled by sending an empty byte array.
//
// "Validation" is based entirely on parentheses counting. The sink must use an
// external mechanism to halt the producer for any reason.
//
// Single threaded, driven by calls to `accept`.
//
// [1] the input arrays may be optionally copied by setting `copyInput` to true,
//     allowing the producer to reuse an array.
final class Chunker(
  maxLength: Int,
  copyInput: Boolean,
  sink: Chunks => Unit
) extends Closeable {
  // the content of the current JSON message. `null` if closed.
  private[this] var subs: List[Sub] = Nil

  // the state of the current JSON message
  private[this] var length: Int = 0
  // string/escaped/parens naturally reset at the beginning of each payload
  private[this] var string: Boolean  = false
  private[this] var escaped: Boolean = false
  private[this] var parens: Int      = 0 // curly or square

  // records if the previous input had a multi-byte character at the end and how
  // many bytes need to be skipped in the next iteration.
  private[this] var multibyteSkip: Int = 0

  def close(): Unit = {
    sink(new Chunks(Nil))
    subs = null
  }

  private def userError(msg: String): Unit = {
    close()
    throw new IllegalArgumentException(s"jzon.Chunker: $msg") with NoStackTrace
  }

  // there's a lot of logic here duplicated with Lexer.skip* but heavily
  // optimised for this usecase, so cannot be feasibly shared.
  def accept(bytes: Array[Byte], bytes_length: Int): Unit = {
    if (subs eq null)
      userError("closed")
    if ((bytes eq null) || bytes.isEmpty)
      return close()

    var i: Int     = multibyteSkip
    var start: Int = 0
    while (i < bytes_length) {
      // multibyte logic duplicated from FastBytesReader
      val a: Int = bytes(i) & 0xFF
      ((a >> 4): @switch) match {
        case 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 =>
          i += 1
          val c = a.toChar

          if (string) {
            if (!escaped && c == '"') string = false
            else if (escaped) escaped = false
            else if (c == '\\') escaped = true
          } else if (c == '{' || c == '[') {
            parens += 1
          } else if (c == '}' || c == ']') {
            parens -= 1
            if (parens == 0) {
              // end of this json object
              store(bytes, start, i)
              sink(new Chunks(subs))
              // start a new message
              subs = Nil
              start = i
              length = 0
            } else if (parens < 0) {
              userError("unbalanced parentheses")
            }
          }
        case 12 | 13 =>
          i += 2
        case 14 =>
          i += 3
        case 15 =>
          i += 4
        case _ =>
          userError(s"invalid UTF-8, bad multi-byte prefix")
      }
    }

    if (multibyteSkip != 0) {
      if (bytes_length < multibyteSkip) {
        multibyteSkip -= bytes_length
      } else {
        multibyteSkip = 0
      }
    }
    if (i > bytes_length) {
      multibyteSkip = i - bytes_length
      i = bytes_length
    }

    if (i != start)
      store(bytes, start, i)
  }

  private[this] def store(bytes: Array[Byte], start: Int, i: Int): Unit = {
    val bytes_ = if (copyInput) bytes.clone() else bytes

    // assert(i > start)
    subs ::= new Sub(bytes_, start, i - start)
    length += subs.length

    if (maxLength > -1 && length > maxLength)
      userError("maximum payload length was exceeded")
  }

}
