package zio.json.internal

import scalaprops._
import Property.{ implies, prop, property }

import zio.json._
import zio.json.syntax._
import zio.json.GenAst._

import utest._

// testOnly *LexerTest*
object LexerProps extends Scalaprops {

  val skipPretty = property { (j: JsValue) =>
    val in  = new FastStringReader(j.toJsonPretty)
    val out = new FastStringWriter(1024)
    Lexer.skipValue(Nil, in, out)
    prop(out.toString == j.toJson)
  }

}

// known bad cases
object LexerTest extends TestSuite {

  val tests = Tests {
    test("ignore strings") {
      val str = "\"\\\"\"" // "\""

      val in  = new FastStringReader(str)
      val out = new FastStringWriter(1024)
      Lexer.skipValue(Nil, in, out)

      out.toString ==> str
    }

    test("geojson1") {
      val str = TestUtils.getResourceAsString("che.geo.json")

      val in  = new FastStringReader(str)
      val out = new FastStringWriter(1024)
      Lexer.skipValue(Nil, in, out)

      out.toString ==> str
    }
  }

}
