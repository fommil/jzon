package jzon

import jzon.internal._
import jzon.syntax._

class EncoderTest extends Test {

  object exampleproducts {
    case class Parameterless()
    object Parameterless {
      implicit val encoder: Encoder[Parameterless] =
        Encoder.derived
    }

    case class OnlyString(s: String)
    object OnlyString {
      implicit val encoder: Encoder[OnlyString] =
        Encoder.derived
    }

    case class CoupleOfThings(@field("j") i: Int, f: Option[Float], b: Boolean)
    object CoupleOfThings {
      implicit val encoder: Encoder[CoupleOfThings] =
        Encoder.derived
    }
  }

  object examplesum {

    sealed abstract class Parent
    case class Child1() extends Parent
    @hint("Cain")
    case class Child2() extends Parent

    object Parent {
      implicit val encoder: Encoder[Parent] = Encoder.derived
    }
    object Child1 {
      implicit val encoder: Encoder[Child1] = Encoder.derived
    }
    object Child2 {
      implicit val encoder: Encoder[Child2] = Encoder.derived
    }
  }

  object examplealtsum {

    @discriminator("hint")
    sealed abstract class Parent
    case class Child1() extends Parent
    @hint("Abel")
    case class Child2(s: Option[String]) extends Parent

    object Parent {
      implicit val encoder: Encoder[Parent] = Encoder.derived
    }
    object Child1 {
      implicit val encoder: Encoder[Child1] = Encoder.derived
    }
    object Child2 {
      implicit val encoder: Encoder[Child2] = Encoder.derived
    }
  }

  object examplegeneric {
    case class Genericy[A](a: A)
    object Genericy {
      implicit def encoder[A: Encoder]: Encoder[Genericy[A]] = Encoder.derived
    }
  }

  def testPrimitives = {
    assertEquals("\"hello world\"", "hello world".toJson)
    assertEquals("\"hello\\nworld\"", "hello\nworld".toJson)
    assertEquals("\"hello\\rworld\"", "hello\rworld".toJson)
    assertEquals("\"hello\\u0000world\"", "hello\u0000world".toJson)

    assertEquals("true", true.toJson)
    assertEquals("false", false.toJson)
    assertEquals("\"c\"", 'c'.toJson)
    assertEquals("\"c\"", Symbol("c").toJson)

    assertEquals("1", (1: Byte).toJson)
    assertEquals("1", (1: Short).toJson)
    assertEquals("1", (1: Int).toJson)
    assertEquals("1", (1L).toJson)
    assertEquals("1", (new java.math.BigInteger("1")).toJson)
    assertEquals(
      "170141183460469231731687303715884105728",
      new java.math.BigInteger("170141183460469231731687303715884105728").toJson
    )

    assertEquals("1.0", (1.0f).toJson)
    assertEquals("1.0", (1.0d).toJson)

    assertEquals("\"NaN\"", Float.NaN.toJson)
    assertEquals("\"Infinity\"", Float.PositiveInfinity.toJson)
    assertEquals("\"-Infinity\"", Float.NegativeInfinity.toJson)

    assertEquals("\"NaN\"", Double.NaN.toJson)
    assertEquals("\"Infinity\"", Double.PositiveInfinity.toJson)
    assertEquals("\"-Infinity\"", Double.NegativeInfinity.toJson)
  }

  def testOptions = {
    assertEquals("null", (None: Option[Int]).toJson)
    assertEquals("1", (Some(1): Option[Int]).toJson)
  }

  def testEithers = {
    assertEquals("""{"Left":1}""", (Left(1): Either[Int, Int]).toJson)
    assertEquals("""{"Right":1}""", (Right(1): Either[Int, Int]).toJson)
    assertEquals("{\n  \"Left\" : 1\n}", (Left(1): Either[Int, Int]).toJsonPretty)
    assertEquals("{\n  \"Right\" : 1\n}", (Right(1): Either[Int, Int]).toJsonPretty)
  }

  def testCollections = {
    assertEquals("[]", List[Int]().toJson)
    assertEquals("[1,2,3]", List(1, 2, 3).toJson)
    assertEquals("[]", Vector[Int]().toJson)
    assertEquals("[1,2,3]", Vector(1, 2, 3).toJson)

    assertEquals("{}", Map[String, String]().toJson)
    assertEquals("""{"hello":"world"}""", Map("hello" -> "world").toJson)
    assertEquals("""{"hello":"world"}""", Map("hello" -> Some("world"), "goodbye" -> None).toJson)

    assertEquals("[]", List[Int]().toJsonPretty)
    assertEquals("[1, 2, 3]", List(1, 2, 3).toJsonPretty)
    assertEquals("[]", Vector[Int]().toJsonPretty)
    assertEquals("[1, 2, 3]", Vector(1, 2, 3).toJsonPretty)

    assertEquals("{}", Map[String, String]().toJsonPretty)
    assertEquals("{\n  \"hello\" : \"world\"\n}", Map("hello" -> "world").toJsonPretty)
    assertEquals("{\n  \"hello\" : \"world\"\n}", Map("hello" -> Some("world"), "goodbye" -> None).toJsonPretty)
  }

  def testParameterlessProducts = {
    import exampleproducts._

    assertEquals("{}", Parameterless().toJson)
    assertEquals("{}", Parameterless().toJsonPretty)
  }

  def testTuples = {
    assertEquals("""["hello","world"]""", ("hello", "world").toJson)
    assertEquals("""["hello", "world"]""", ("hello", "world").toJsonPretty)
  }

  def testProducts = {
    import exampleproducts._

    assertEquals("""{"s":"foo"}""", OnlyString("foo").toJson)
    assertEquals("""{"j":-1,"f":10.0,"b":false}""", CoupleOfThings(-1, Some(10.0f), false).toJson)
    assertEquals("""{"j":0,"b":true}""", CoupleOfThings(0, None, true).toJson)

    assertEquals("{\n  \"s\" : \"foo\"\n}", OnlyString("foo").toJsonPretty)
    assertEquals(
      "{\n  \"j\" : -1,\n  \"f\" : 10.0,\n  \"b\" : false\n}",
      CoupleOfThings(-1, Some(10.0f), false).toJsonPretty
    )
    assertEquals("{\n  \"j\" : 0,\n  \"b\" : true\n}", CoupleOfThings(0, None, true).toJsonPretty)
  }

  def testSumEncoding = {
    import examplesum._

    assertEquals("""{"Child1":{}}""", (Child1(): Parent).toJson)
    assertEquals("""{"Cain":{}}""", (Child2(): Parent).toJson)

    assertEquals("{\n  \"Child1\" : {}\n}", (Child1(): Parent).toJsonPretty)
    assertEquals("{\n  \"Cain\" : {}\n}", (Child2(): Parent).toJsonPretty)
  }

  def testSumAlternativeEncoding = {
    import examplealtsum._

    assertEquals("""{"hint":"Child1"}""", (Child1(): Parent).toJson)
    assertEquals("""{"hint":"Abel"}""", (Child2(None): Parent).toJson)
    assertEquals("""{"hint":"Abel","s":"hello"}""", (Child2(Some("hello")): Parent).toJson)

    // note lack of whitespace on last line
    assertEquals("{\n  \"hint\" : \"Child1\"}", (Child1(): Parent).toJsonPretty)
    assertEquals("{\n  \"hint\" : \"Abel\"}", (Child2(None): Parent).toJsonPretty)
    assertEquals("{\n  \"hint\" : \"Abel\",\n  \"s\" : \"hello\"\n}", (Child2(Some("hello")): Parent).toJsonPretty)
  }

}
