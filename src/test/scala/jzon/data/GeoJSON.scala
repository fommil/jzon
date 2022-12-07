package jzon.data.geojson

@jzon.discriminator("type")
sealed abstract class Geometry
final case class Point(coordinates: (Double, Double))                          extends Geometry
final case class MultiPoint(coordinates: List[(Double, Double)])               extends Geometry
final case class LineString(coordinates: List[(Double, Double)])               extends Geometry
final case class MultiLineString(coordinates: List[List[(Double, Double)]])    extends Geometry
final case class Polygon(coordinates: List[List[(Double, Double)]])            extends Geometry
final case class MultiPolygon(coordinates: List[List[List[(Double, Double)]]]) extends Geometry
final case class GeometryCollection(
  geometries: List[Geometry] // NOTE: recursive
) extends Geometry

@jzon.discriminator("type")
sealed abstract class GeoJSON
final case class Feature(properties: Map[String, String], geometry: Geometry) extends GeoJSON
final case class FeatureCollection(
  features: List[GeoJSON] // NOTE: recursive
) extends GeoJSON

object Point {
  implicit val decoder: jzon.Decoder[Point] = jzon.Decoder.derived
  implicit val encoder: jzon.Encoder[Point] = jzon.Encoder.derived
}
object MultiPoint {
  implicit val decoder: jzon.Decoder[MultiPoint] = jzon.Decoder.derived
  implicit val encoder: jzon.Encoder[MultiPoint] = jzon.Encoder.derived
}
object LineString {
  implicit val decoder: jzon.Decoder[LineString] = jzon.Decoder.derived
  implicit val encoder: jzon.Encoder[LineString] = jzon.Encoder.derived
}
object MultiLineString {
  implicit val decoder: jzon.Decoder[MultiLineString] = jzon.Decoder.derived
  implicit val encoder: jzon.Encoder[MultiLineString] = jzon.Encoder.derived
}
object Polygon {
  implicit val decoder: jzon.Decoder[Polygon] = jzon.Decoder.derived
  implicit val encoder: jzon.Encoder[Polygon] = jzon.Encoder.derived
}
object MultiPolygon {
  implicit val decoder: jzon.Decoder[MultiPolygon] = jzon.Decoder.derived
  implicit val encoder: jzon.Encoder[MultiPolygon] = jzon.Encoder.derived
}
object GeometryCollection {
  implicit val decoder: jzon.Decoder[GeometryCollection] = jzon.Decoder.derived
  implicit val encoder: jzon.Encoder[GeometryCollection] = jzon.Encoder.derived
}
object Feature {
  implicit val decoder: jzon.Decoder[Feature] = jzon.Decoder.derived
  implicit val encoder: jzon.Encoder[Feature] = jzon.Encoder.derived
}
object FeatureCollection {
  implicit val decoder: jzon.Decoder[FeatureCollection] = jzon.Decoder.derived
  implicit val encoder: jzon.Encoder[FeatureCollection] = jzon.Encoder.derived
}

object Geometry {
  implicit val jzonDecoder: jzon.Decoder[Geometry]      = jzon.Decoder.derived
  implicit lazy val jzonEncoder: jzon.Encoder[Geometry] = jzon.Encoder.derived
}
object GeoJSON {
  implicit val jzonDecoder: jzon.Decoder[GeoJSON]      = jzon.Decoder.derived
  implicit lazy val jzonEncoder: jzon.Encoder[GeoJSON] = jzon.Encoder.derived
}
