package zio.json.internal

import java.io.InputStream

final class Sub(
  val bytes: Array[Byte],
  val offset: Int,
  val length: Int
)
final class Chunks(
  // subs are in reverse order
  val subs: List[Sub]
) {
  def isEmpty: Boolean = subs == Nil

  private def length: Int = subs.map(_.length).sum
  def toArray: Array[Byte] = {
    val bytes = Array.ofDim[Byte](length)
    var i     = bytes.length
    subs.foreach { s =>
      System.arraycopy(s.bytes, s.offset, bytes, i - s.length, s.length)
      i -= s.length
    }
    bytes
  }
}

// unsynchronized alternative to InputStream
trait ByteInput {
  def read(): Int // -1 if EOF, otherwise the byte
  def eof(): Boolean
}

final class ByteArrayInput(bytes: Array[Byte]) extends InputStream with ByteInput {
  private[this] var i: Int = 0

  @inline override def eof(): Boolean = i >= bytes.length

  override def read(): Int =
    if (eof()) -1
    else {
      val res = bytes(i) & 0xFF
      i += 1
      res
    }
}

final class ChunksInput(chunks: Chunks) extends InputStream with ByteInput {
  private[this] var subs = chunks.subs.reverse
  // chunks no longer referenced, as each Sub is consumed it can be GCed
  private[this] var i: Int = 0

  override def read(): Int =
    if (subs == Nil) -1
    else {
      val s = subs.head
      // trust the input not to be non-empty and not out of bounds
      val b = s.bytes(s.offset + i) & 0xFF
      i += 1
      if (i >= s.length) {
        subs = subs.tail
        i = 0
      }
      b
    }

  override def eof(): Boolean = subs == Nil
}
