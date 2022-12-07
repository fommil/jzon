package jzon.data.googlemaps

final case class Value(
  text: String,
  @jzon.field("value")
  v: Int
)
final case class Elements(distance: Value, duration: Value, status: String)
final case class Rows(elements: List[Elements])
// @jzon.no_extra_fields // entirely mitigates Attack1
final case class DistanceMatrix(
  destination_addresses: List[String],
  origin_addresses: List[String],
  rows: List[Rows],
  status: String
)

object Value {
  implicit val jzonDecoder: jzon.Decoder[Value] = jzon.Decoder.derived
  implicit val jzonEncoder: jzon.Encoder[Value] = jzon.Encoder.derived
}
object Elements {
  implicit val jzonDecoder: jzon.Decoder[Elements] = jzon.Decoder.derived
  implicit val jzonEncoder: jzon.Encoder[Elements] = jzon.Encoder.derived
}
object Rows {
  implicit val jzonDecoder: jzon.Decoder[Rows] = jzon.Decoder.derived
  implicit val jzonEncoder: jzon.Encoder[Rows] = jzon.Encoder.derived
}
object DistanceMatrix {
  implicit val jzonDecoder: jzon.Decoder[DistanceMatrix] = jzon.Decoder.derived
  implicit val jzonEncoder: jzon.Encoder[DistanceMatrix] = jzon.Encoder.derived
}
