package zio.json

import zio.json
import zio.json.syntax._
import scalaprops._
import Property.{ implies, prop, property }

import GenAst._

// testOnly *RoundtripTest
object RoundtripTest extends Scalaprops {

  def roundtrip[A: Encoder: Decoder](a: A) =
    prop(json.parser.decode[A](a.toJson) == Right(a)) and
      prop(json.parser.decode[A](a.toJsonPretty) == Right(a))

  // arbitrary strings are not guaranteed to roundtrip due to normalisation of
  // some unicode characters, but we could still test this on a subset of
  // strings if we wanted to create the Gens

  val booleans = property { i: Boolean => roundtrip(i) }

  val bytes   = property { i: Byte => roundtrip(i) }
  val shorts  = property { i: Short => roundtrip(i) }
  val ints    = property { i: Int => roundtrip(i) }
  val longs   = property { i: Long => roundtrip(i) }
  val bigints = property { i: java.math.BigInteger => implies(i.bitLength < 128, roundtrip(i)) }

  // NaN / Infinity is tested manually, because of == semantics
  val floats  = property { i: Float => implies(i.isFinite, roundtrip(i)) }
  val doubles = property { i: Double => implies(i.isFinite, roundtrip(i)) }

  val asts = property { i: JsValue => roundtrip(i) }

}
