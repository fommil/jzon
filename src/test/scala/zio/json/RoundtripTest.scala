package zio.json

import java.io.File
import java.nio.file.Files
import java.nio.file.Path
import zio.json.syntax._
import zio.json.internal._

class RoundtripTest extends Test {

  def roundtrip[A: Decoder: Encoder](a: A): Unit = {
    assertEquals(Right(a), parser.decode(a.toJson))
    assertEquals(Right(a), parser.decode(a.toJsonPretty))
  }

  def roundtripFile[A: Decoder: Encoder](res: String, pretty: Boolean): Unit = {
    val input          = TestUtils.getResourceAsString(res)
    val Right(decoded) = parser.decode[A](input)
    val recoded        = if (pretty) decoded.toJsonPretty else decoded.toJson

    // if this test fails, it's a lot easier to look at a diff
    if (recoded.replaceAll("\\s", "") != input.replaceAll("\\s", "")) {
      Files.writeString(Path.of(res).getFileName, recoded)
      assert(false, res)
    }
  }

  // arbitrary strings are not guaranteed to roundtrip due to normalisation of
  // some unicode characters, but we could still test this on a subset of
  // strings if we wanted to create the Gens

  def testBooleans    = Gen.prop(Gen.boolean)(roundtrip(_))
  def testBytes       = Gen.prop(Gen.byte)(roundtrip(_))
  def testShorts      = Gen.prop(Gen.short)(roundtrip(_))
  def testInts        = Gen.prop(Gen.int)(roundtrip(_))
  def testLongs       = Gen.prop(Gen.long)(roundtrip(_))
  def testBigIntegers = Gen.prop(Gen.biginteger(128))(roundtrip(_))
  def testFloats = Gen.prop(Gen.float) { i =>
    if (java.lang.Float.isFinite(i))
      roundtrip(i)
  }
  def testDoubles = Gen.prop(Gen.double) { i =>
    if (java.lang.Double.isFinite(i))
      roundtrip(i)
  }

  def testAsts = Gen.prop(GenAst.ast)(roundtrip(_))

  def testGoogleMaps = {
    import zio.json.data.googlemaps._

    roundtripFile[DistanceMatrix]("google_maps_api_response.json", pretty = true)
  }

  def testTwitter = {
    import zio.json.data.twitter._

    roundtripFile[List[Tweet]]("twitter_api_response.json", pretty = true)
  }

  def testGeoJSON = {
    import zio.json.data.geojson._

    roundtripFile[GeoJSON]("che.geo.json", pretty = false)
  }

}
