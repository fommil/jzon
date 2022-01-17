package zio.json.data.googlemaps

import zio.json

final case class Value(
  text: String,
  @json.field("value")
  v: Int
)
final case class Elements(distance: Value, duration: Value, status: String)
final case class Rows(elements: List[Elements])
// @json.no_extra_fields // entirely mitigates Attack1
final case class DistanceMatrix(
  destination_addresses: List[String],
  origin_addresses: List[String],
  rows: List[Rows],
  status: String
)

object Value {
  implicit val zioJsonDecoder: json.Decoder[Value] = json.Decoder.derived
  implicit val zioJsonEncoder: json.Encoder[Value] = json.Encoder.derived
}
object Elements {
  implicit val zioJsonDecoder: json.Decoder[Elements] = json.Decoder.derived
  implicit val zioJsonEncoder: json.Encoder[Elements] = json.Encoder.derived
}
object Rows {
  implicit val zioJsonDecoder: json.Decoder[Rows] = json.Decoder.derived
  implicit val zioJsonEncoder: json.Encoder[Rows] = json.Encoder.derived
}
object DistanceMatrix {
  implicit val zioJsonDecoder: json.Decoder[DistanceMatrix] = json.Decoder.derived
  implicit val zioJsonEncoder: json.Encoder[DistanceMatrix] = json.Encoder.derived
}
