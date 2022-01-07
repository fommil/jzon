package zio.json.internal

import java.util.Arrays
import scala.annotation.switch
import scala.util.control.NoStackTrace

import zio.json.Decoder.{ JsonError, UnsafeJson }

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

  /** Behaviour is undefined if called more than once without a read() */
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

// TODO FastBytesReader
