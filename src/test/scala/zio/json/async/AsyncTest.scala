package zio.json.async

import zio.json._
import zio.json.internal._
import zio.json.syntax._

class AsyncTest extends Test {

  def testOneMessage: Unit = Gen.prop(GenAst.objects) { ast =>
    val string   = ast.toJson
    val payloads = parse(string, 16)
    assertEquals(List(string, ""), payloads)
  }

  def testMultipleMessages: Unit = Gen.prop(Gen.list(GenAst.objects, 10)) { asts =>
    val strings  = asts.map(_.toJson)
    val payloads = parse(strings.mkString, 16)
    assertEquals(strings :+ "", payloads)
  }

  def testMultibyteBoundary: Unit = {
    val string   = """{"monkey":"🐒"}"""
    val payloads = parse(string, 12)

    assertEquals(List(string, ""), payloads)
  }

  def testGoogleMaps: Unit = {
    import zio.json.data.googlemaps._

    val string  = TestUtils.getResourceAsString("google_maps_api_compact_response.json").trim
    val buffers = Array.fill(2)(string).mkString("\n").getBytes("utf-8").grouped(1024)

    var decoded: List[DistanceMatrix] = Nil
    val work: Chunks => Unit = { c =>
      if (!c.isEmpty) {
        Decoder[DistanceMatrix].decodeJson(c) match {
          case Left(err) => assert(false, err)
          case Right(dm) => decoded ::= dm
        }
      }
    }
    val chunker = new Chunker(string.length + 1, false, work)
    buffers.foreach(b => chunker.accept(b, b.length))
    chunker.accept(null, -1)

    assertEquals(2, decoded.size)
  }

  private def parse(string: String, buffer: Int): List[String] = {
    val bytes = string.getBytes("utf-8")
    val parts = bytes.grouped(buffer)

    var recovered: List[Chunks] = Nil
    val work                    = (c: Chunks) => recovered ::= c
    val chunker                 = new Chunker(bytes.length, false, work)
    parts.foreach(b => chunker.accept(b, b.length))
    chunker.accept(null, -1)

    recovered.reverse.map(chunks => new String(chunks.toArray, "utf-8"))
  }

}
