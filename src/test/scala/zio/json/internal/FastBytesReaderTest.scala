package zio.json.internal

class FastBytesReaderTest extends Test {

  def assertParse(str: String, retractAt: Int): Unit = {
    val in      = new FastBytesReader(new ByteArrayInput(str.getBytes("utf-8")))
    val builder = new FastStringWriter(str.length)
    var i       = 0

    var lowSurrogate = false
    while (i < str.length) {
      if (i == retractAt && i > 0 && !lowSurrogate) {
        in.retract()
        in.readChar()
      }
      val c = in.readChar()
      lowSurrogate = Character.isLowSurrogate(c)
      builder.append(c)
      i += 1
    }
    val parsed = builder.toString

    if (parsed != str) {
      assert(
        false,
        s"FAIL ($parsed) != ($str) | ${parsed.toList.map(_.toLong.toHexString)} ${str.toList.map(_.toLong.toHexString)}"
      )
    }
  }

  def testRebuildString = Gen.prop(Gen.unicode(), Gen.int) { (str: String, num: Int) =>
    val retractAt = if (str.isEmpty) -1 else (num.abs % str.length)
    assertParse(str, retractAt)
  }

  // this is covered by the prop tests but is a useful standalone test
  def testSurrogatePairs = {
    // 0x10348 into 0xd800, 0xdf48
    val str = "wibble wobble I'm a %c (Hwair)".format(0x10348)
    assertParse(str, -1)
  }

  // the character in this test is not a valid character on its own, being both
  // a surrogate and a BMP. It's unclear what the utf-8 / utf-16 encoding of
  // these things are.
  def ignoreBizarroCodepoints = {
    val str = "%c".format(0xdf87)

    // println(str.getBytes("utf-8").toList.map(_.toLong.toHexString))
    // => 0x3F

    assertParse(str, -1)
  }

}
