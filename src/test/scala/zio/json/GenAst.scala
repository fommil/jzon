package zio.json

import zio.json
import zio.json.syntax._
import scalaprops._

// testOnly *RoundtripTest
object GenAst {
  implicit val floatShrinker: Shrink[Float]   = Shrink.empty
  implicit val doubleShrinker: Shrink[Double] = Shrink.empty

  implicit lazy val astGen: Gen[JsValue] = Gen.sized { size =>
    val entry: Gen[(String, JsValue)] = Gen.delay(Gen.apply2(Gen.asciiString, astGen)((a, b) => (a, b)))
    // objects and arrays should get smaller with depth to avoid infinite recursion
    val size_             = 0 min (size - 1)
    val obj: Gen[JsValue] = Gen.delay(Gen.listOfN(size_, entry)).map(JsObject(_))
    val arr: Gen[JsValue] = Gen.delay(Gen.listOfN(size_, astGen)).map(JsArray(_))
    val boo: Gen[JsValue] = Gen[Boolean].map(JsBoolean(_))
    val str: Gen[JsValue] = Gen.asciiString.map(JsString(_))
    val num: Gen[JsValue] = for {
      num <- Gen[java.math.BigDecimal]
      // fallback to null if we ever get a number that is too big
    } yield
      if (num.unscaledValue.bitLength > 128) JsNull
      else JsNumber(num)

    val nul: Gen[JsValue] = Gen.value(JsNull)

    Gen.oneOf(obj, arr, boo, str, num)
  }

  implicit val strShrinker: Shrink[String] = Shrink.shrink { txt =>
    if (txt.isEmpty) Stream.empty[String]
    else Stream(txt.drop(1), txt.reverse.drop(1).reverse)
  }

  implicit lazy val astShrinker: Shrink[JsValue] = Shrink.shrink {
    case JsObject(entries) => Shrink.list[(String, JsValue)].apply(entries).map(JsObject(_))
    case JsArray(entries)  => Shrink.list[JsValue].apply(entries).map(JsArray(_))
    case JsBoolean(_)      => Stream.empty[JsValue]
    case JsString(txt)     => strShrinker(txt).map(JsString(_))
    case JsNumber(_)       => Stream.empty[JsValue]
    case JsNull            => Stream.empty[JsValue]
  }
}
