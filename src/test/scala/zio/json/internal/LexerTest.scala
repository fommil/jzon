package zio.json.internal

import zio.json.syntax._

class LexerTest extends Test {

  def testPrettyParsesPolitely = Gen.prop(GenAst.ast) { j =>
    val in  = new FastStringReader(j.toJsonPretty)
    val out = new FastStringWriter(1024)
    Lexer.skipValue(Nil, in, out)
    assertEquals(j.toJson, out.toString)
  }

  def testIgnoreStrings = {
    val str = "\"\\\"\"" // "\""

    val in  = new FastStringReader(str)
    val out = new FastStringWriter(1024)
    Lexer.skipValue(Nil, in, out)

    assertEquals(str, out.toString)
  }

  def testGeojson1 = {
    val str = TestUtils.getResourceAsString("che.geo.json")

    val in  = new FastStringReader(str)
    val out = new FastStringWriter(1024)
    Lexer.skipValue(Nil, in, out)

    assertEquals(str, out.toString)
  }

}
