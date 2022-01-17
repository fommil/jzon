package zio.json.data.geojson

import zio.json

@json.discriminator("type")
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

@json.discriminator("type")
sealed abstract class GeoJSON
final case class Feature(properties: Map[String, String], geometry: Geometry) extends GeoJSON
final case class FeatureCollection(
  features: List[GeoJSON] // NOTE: recursive
) extends GeoJSON

object Point {
  implicit val decoder: json.Decoder[Point] = json.Decoder.derived
  implicit val encoder: json.Encoder[Point] = json.Encoder.derived
}
object MultiPoint {
  implicit val decoder: json.Decoder[MultiPoint] = json.Decoder.derived
  implicit val encoder: json.Encoder[MultiPoint] = json.Encoder.derived
}
object LineString {
  implicit val decoder: json.Decoder[LineString] = json.Decoder.derived
  implicit val encoder: json.Encoder[LineString] = json.Encoder.derived
}
object MultiLineString {
  implicit val decoder: json.Decoder[MultiLineString] = json.Decoder.derived
  implicit val encoder: json.Encoder[MultiLineString] = json.Encoder.derived
}
object Polygon {
  implicit val decoder: json.Decoder[Polygon] = json.Decoder.derived
  implicit val encoder: json.Encoder[Polygon] = json.Encoder.derived
}
object MultiPolygon {
  implicit val decoder: json.Decoder[MultiPolygon] = json.Decoder.derived
  implicit val encoder: json.Encoder[MultiPolygon] = json.Encoder.derived
}
object GeometryCollection {
  implicit val decoder: json.Decoder[GeometryCollection] = json.Decoder.derived
  implicit val encoder: json.Encoder[GeometryCollection] = json.Encoder.derived
}
object Feature {
  implicit val decoder: json.Decoder[Feature] = json.Decoder.derived
  implicit val encoder: json.Encoder[Feature] = json.Encoder.derived
}
object FeatureCollection {
  implicit val decoder: json.Decoder[FeatureCollection] = json.Decoder.derived
  implicit val encoder: json.Encoder[FeatureCollection] = json.Encoder.derived
}

object Geometry {
  implicit val zioJsonDecoder: json.Decoder[Geometry]      = json.Decoder.derived
  implicit lazy val zioJsonEncoder: json.Encoder[Geometry] = json.Encoder.derived
}
object GeoJSON {
  implicit val zioJsonDecoder: json.Decoder[GeoJSON]      = json.Decoder.derived
  implicit lazy val zioJsonEncoder: json.Encoder[GeoJSON] = json.Encoder.derived
}
