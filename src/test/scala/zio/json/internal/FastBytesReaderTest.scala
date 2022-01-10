package zio.json.internal

import scalaprops._
import Property.{ implies, prop, property }
import utest._

object FastBytesReaderProps extends Scalaprops {
  override val param = super.param.copy(maxSize = 100000)

  // samples from all valid utf-8 values, including those that are represented
  // as multiple Chars (surrogate pairs) in Java's utf-16 encoding.
  //
  // https://github.com/scalaprops/scalaprops/issues/26
  val unicodeString: Gen[String] = {
    val chars = Gen.choose(0, 0x10FFFF).map { cp =>
      if (Character.isBmpCodePoint(cp)) List(cp.toChar)
      else List(Character.highSurrogate(cp), Character.lowSurrogate(cp))
    }
    Gen.listOf(chars).map(_.flatten.mkString)
  }

  // only covers the Basic Multilingual Plane, additional tests needed for
  // surrogate pairs.
  val genBmp = Gen.choose(0, 0xD7FF).map(_.toChar)

  implicit val genBmpString: Gen[String] = Gen.genString(genBmp, 0)
  implicit val strShrinker: Shrink[String] = Shrink.shrink { txt =>
    if (txt.isEmpty) Stream.empty[String]
    else if (txt.length == 1) Stream("")
    else {
      val a = if (txt.head.isSurrogate) txt.drop(2) else txt.drop(1)
      val b = if (txt.last.isSurrogate) txt.take(txt.length - 2) else txt.take(txt.length - 1)
      Stream(a, b).filter(_ != txt).distinct
    }
  }

  val safe = property { (str: String, num: Int) =>
    val retractAt = if (str.isEmpty) -1 else (num % str.length)
    val parsed    = parse(str, retractAt)

    if (parsed != str)
      println(s"FAIL ($parsed) != ($str) | ${parsed.toList} ${str.toList}")

    prop(parsed == str)
  }

  def parse(str: String, retractAt: Int): String = {
    val in      = new FastBytesReader(str.getBytes("utf-8"))
    val builder = new FastStringWriter(str.length)
    var i       = 0
    while (i < str.length) {
      if (i == retractAt && i > 0) {
        in.retract()
        in.readChar()
      }
      builder.append(in.readChar())
      i += 1
    }
    builder.toString
  }
}

object FastBytesReaderTest extends TestSuite {

  val tests = Tests {
    // this is covered by the prop tests but is a useful standalone test
    test("surrogate pairs") {
      // 0x10348 into 0xd800, 0xdf48
      val str = "wibble wobble I'm a %c (Hwair)".format(0x10348)
      FastBytesReaderProps.parse(str, -1) ==> str
    }
  }

}
