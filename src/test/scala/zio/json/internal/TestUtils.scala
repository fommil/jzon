package zio.json.internal

import zio.json._

abstract class Test extends junit.framework.TestCase {
  final def assertEquals[A](expected: A, got: A): Unit =
    assert(expected == got, s"$got != $expected")
}

object TestUtils {
  // by plokhotnyuk
  def zeroHashCodeStrings: Iterator[String] = {
    def charAndHash(h: Int): Iterator[(Char, Int)] = ('!' to '~').iterator.map(ch => (ch, (h + ch) * 31))

    for {
      (ch0, h0) <- charAndHash(0)
      (ch1, h1) <- charAndHash(h0)
      (ch2, h2) <- charAndHash(h1) if (((h2 + 32) * 923521) ^ ((h2 + 127) * 923521)) < 0
      (ch3, h3) <- charAndHash(h2) if (((h3 + 32) * 29791) ^ ((h3 + 127) * 29791)) < 0
      (ch4, h4) <- charAndHash(h3) if (((h4 + 32) * 961) ^ ((h4 + 127) * 961)) < 0
      (ch5, h5) <- charAndHash(h4) if (((h5 + 32) * 31) ^ ((h5 + 127) * 31)) < 0
      (ch6, h6) <- charAndHash(h5) if ((h6 + 32) ^ (h6 + 127)) < 0
      (ch7, _)  <- charAndHash(h6) if h6 + ch7 == 0
    } yield new String(Array(ch0, ch1, ch2, ch3, ch4, ch5, ch6, ch7))
  }

  def writeFile(path: String, s: String): Unit = {
    val bw = new java.io.BufferedWriter(new java.io.FileWriter(path))
    bw.write(s)
    bw.close()
  }

  def getResourceAsString(res: String): String = {
    val is = getClass.getClassLoader.getResourceAsStream(res)
    try {
      val baos        = new java.io.ByteArrayOutputStream()
      val data        = Array.ofDim[Byte](2048)
      var len: Int    = 0
      def read(): Int = { len = is.read(data); len }
      while (read() != -1)
        baos.write(data, 0, len)
      baos.toString("UTF-8")
    } finally is.close()
  }

  def asBytes(str: String): Array[Byte] = str.getBytes("utf-8")

  def getResourceAsReader(res: String): RetractReader =
    new internal.FastStringReader(getResourceAsString(res))

}
