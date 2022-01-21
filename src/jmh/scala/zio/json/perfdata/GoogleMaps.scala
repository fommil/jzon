package zio.json.perfdata.googlemaps

import com.github.plokhotnyuk.jsoniter_scala.macros.named
import io.circe
import zio.json
import play.api.libs.{ json => Play }

final case class Value(
  text: String,
  @named("value")
  @json.field("value")
  @circe.generic.extras.JsonKey("value")
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

  implicit val customConfig: circe.generic.extras.Configuration =
    circe.generic.extras.Configuration.default
  implicit val circeDecoder: circe.Decoder[Value] =
    circe.generic.extras.semiauto.deriveConfiguredDecoder[Value]
  implicit val circeEncoder: circe.Encoder[Value] =
    circe.generic.extras.semiauto.deriveConfiguredEncoder[Value]

  // play macros don't support custom field
  // implicit val playDecoder: Play.Reads[Value] = Play.Json.reads[Value]

  implicit val playDecoder: Play.Reads[Value] = {
    import play.api.libs.json._
    import play.api.libs.json.Reads._
    import play.api.libs.functional.syntax._

    ((JsPath \ "text").read[String].and((JsPath \ "value").read[Int]))(
      Value.apply _
    )
  }
  implicit val playEncoder: Play.Writes[Value] = {
    import play.api.libs.json._
    import play.api.libs.json.Writes._
    import play.api.libs.functional.syntax._

    ((JsPath \ "text").write[String].and((JsPath \ "value").write[Int]))(unlift(Value.unapply))
  }

}
object Elements {
  implicit val zioJsonDecoder: json.Decoder[Elements] = json.Decoder.derived
  implicit val zioJsonEncoder: json.Encoder[Elements] = json.Encoder.derived

  implicit val customConfig: circe.generic.extras.Configuration =
    circe.generic.extras.Configuration.default
  implicit val circeDecoder: circe.Decoder[Elements] =
    circe.generic.extras.semiauto.deriveConfiguredDecoder[Elements]
  implicit val circeEncoder: circe.Encoder[Elements] =
    circe.generic.extras.semiauto.deriveConfiguredEncoder[Elements]

  implicit val playDecoder: Play.Reads[Elements]  = Play.Json.reads[Elements]
  implicit val playEncoder: Play.Writes[Elements] = Play.Json.writes[Elements]

}
object Rows {
  implicit val zioJsonDecoder: json.Decoder[Rows] = json.Decoder.derived
  implicit val zioJsonEncoder: json.Encoder[Rows] = json.Encoder.derived

  implicit val customConfig: circe.generic.extras.Configuration =
    circe.generic.extras.Configuration.default
  implicit val circeDecoder: circe.Decoder[Rows] =
    circe.generic.extras.semiauto.deriveConfiguredDecoder[Rows]
  implicit val circeEncoder: circe.Encoder[Rows] =
    circe.generic.extras.semiauto.deriveConfiguredEncoder[Rows]

  implicit val playDecoder: Play.Reads[Rows]  = Play.Json.reads[Rows]
  implicit val playEncoder: Play.Writes[Rows] = Play.Json.writes[Rows]

}
object DistanceMatrix {
  implicit val zioJsonDecoder: json.Decoder[DistanceMatrix] = json.Decoder.derived
  implicit val zioJsonEncoder: json.Encoder[DistanceMatrix] = json.Encoder.derived

  implicit val customConfig: circe.generic.extras.Configuration =
    circe.generic.extras.Configuration.default
  implicit val circeDecoder: circe.Decoder[DistanceMatrix] =
    circe.generic.extras.semiauto.deriveConfiguredDecoder[DistanceMatrix]
  implicit val circeEncoder: circe.Encoder[DistanceMatrix] =
    circe.generic.extras.semiauto.deriveConfiguredEncoder[DistanceMatrix]

  implicit val playDecoder: Play.Reads[DistanceMatrix] =
    Play.Json.reads[DistanceMatrix]
  implicit val playEncoder: Play.Writes[DistanceMatrix] =
    Play.Json.writes[DistanceMatrix]

}
