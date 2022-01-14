package zio.json.compat

import zio.json, json.syntax._
import zio.json.TestUtils._
import eu.timepit.refined.api.{ Refined }

import eu.timepit.refined.api.Refined
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.auto._

import utest._
import zio.json.compat.refined._

// testOnly *RefinedTest
object RefinedTest extends TestSuite {

  case class Person(name: String Refined NonEmpty)

  object Person {
    implicit val decoder: json.Decoder[Person] = json.Decoder.derived
    implicit val encoder: json.Encoder[Person] = json.Encoder.derived
  }

  val tests = Tests {
    test("Refined") {
      json.parser.decode[Person]("""{"name":""}""") ==> Left(".name(Predicate isEmpty() did not fail.)")
      json.parser.decode[Person]("""{"name":"fommil"}""") ==> Right(Person("fommil"))

      Person("fommil").toJson ==> """{"name":"fommil"}"""
    }
  }

}
