package zio.json.compat

import zio.json._
import zio.json.internal._
import zio.json.syntax._

import eu.timepit.refined.api.Refined
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.auto._

import zio.json.compat.refined._

class RefinedTest extends Test {

  case class Person(name: String Refined NonEmpty)

  object Person {
    implicit val decoder: Decoder[Person] = Decoder.derived
    implicit val encoder: Encoder[Person] = Encoder.derived
  }

  def testRefined = {
    assertEquals(
      Left(".name(Predicate isEmpty() did not fail.)"),
      parser.decode[Person]("""{"name":""}""")
    )

    assertEquals(
      Right(Person("fommil")),
      parser.decode[Person]("""{"name":"fommil"}""")
    )

    assertEquals(
      """{"name":"fommil"}""",
      Person("fommil").toJson
    )
  }

}
