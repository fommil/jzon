package jzon

import scala.collection.immutable
import jzon.internal._

class DecoderTest extends Test {

  object exampleproducts {
    case class Parameterless()
    @no_extra_fields
    case class OnlyString(s: String)

    object Parameterless {
      implicit val decoder: Decoder[Parameterless] = Decoder.derived
    }
    object OnlyString {
      implicit val decoder: Decoder[OnlyString] = Decoder.derived
    }
  }

  object examplesum {

    sealed abstract class Parent
    case class Child1() extends Parent
    case class Child2() extends Parent

    object Parent {
      implicit val decoder: Decoder[Parent] = Decoder.derived
    }
    object Child1 {
      implicit val decoder: Decoder[Child1] = Decoder.derived
    }
    object Child2 {
      implicit val decoder: Decoder[Child2] = Decoder.derived
    }
  }

  object examplealtsum {

    @discriminator("hint")
    sealed abstract class Parent
    @hint("Cain")
    case class Child1() extends Parent
    @hint("Abel")
    case class Child2(@field("lamb") sheep: Int) extends Parent

    object Parent {
      implicit val decoder: Decoder[Parent] = Decoder.derived
    }
    object Child1 {
      implicit val decoder: Decoder[Child1] = Decoder.derived
    }
    object Child2 {
      implicit val decoder: Decoder[Child2] = Decoder.derived
    }
  }

  case class Stringy(value: String)
  object Stringy {
    implicit val decoder: FieldDecoder[Stringy] = FieldDecoder[String].map(Stringy(_))
  }

  def testPrimitives =
    // this big integer consumes more than 128 bits
    assertEquals(
      Left("(expected a 128 bit BigInteger)"),
      parser.decode[java.math.BigInteger]("170141183460469231731687303715884105728")
    )

  def testEithers = {
    val bernies = List("""{"a":1}""", """{"left":1}""", """{"Left":1}""")
    val trumps  = List("""{"b":2}""", """{"right":2}""", """{"Right":2}""")

    bernies.foreach(s => assertEquals(Right(Left(1)), parser.decode[Either[Int, Int]](s)))
    trumps.foreach(s => assertEquals(Right(Right(2)), parser.decode[Either[Int, Int]](s)))
  }

  def testParameterlessProducts = {
    import exampleproducts._

    assertEquals(
      Right(Parameterless()),
      parser.decode[Parameterless]("""{}""")
    )

    // actually anything works... consider this a canary test because if only
    // the empty object is supported that's fine.
    assertEquals(Right(Parameterless()), parser.decode[Parameterless]("""null"""))
    assertEquals(Right(Parameterless()), parser.decode[Parameterless]("""{"field":"value"}"""))
  }

  def testNoExtraFields = {
    import exampleproducts._

    assertEquals(
      Right(OnlyString("")),
      parser.decode[OnlyString]("""{"s":""}""")
    )

    assertEquals(
      Left("(invalid extra field)"),
      parser.decode[OnlyString]("""{"s":"","t":""}""")
    )
  }

  def testSumEncoding = {
    import examplesum._

    assertEquals(Right(Child1()), parser.decode[Parent]("""{"Child1":{}}"""))
    assertEquals(Right(Child2()), parser.decode[Parent]("""{"Child2":{}}"""))
    assertEquals(Left("(invalid disambiguator)"), parser.decode[Parent]("""{"type":"Child1"}"""))
  }

  def testSumAlternativeEncoding = {
    import examplealtsum._

    assertEquals(Right(Child1()), parser.decode[Parent]("""{"hint":"Cain"}"""))
    assertEquals(Right(Child2(1)), parser.decode[Parent]("""{"hint":"Abel", "lamb":1}"""))
    assertEquals(Left("(invalid disambiguator in 'hint')"), parser.decode[Parent]("""{"hint":"Samson"}"""))
    assertEquals(Left("(missing disambiguator 'hint')"), parser.decode[Parent]("""{"Cain":{}}"""))
    assertEquals(Left("(duplicate disambiguator 'hint')"), parser.decode[Parent]("""{"hint":"Cain", "hint":"Cain"}"""))
  }

  def testUnicode =
    assertEquals(Right("€🐵🥰"), parser.decode[String](""""€🐵🥰""""))

  // collections tests contributed by Piotr Paradziński
  def testSeq = {
    val jsonStr  = """["5XL","2XL","XL"]"""
    val expected = Seq("5XL", "2XL", "XL")
    assertEquals(Right(expected), parser.decode[Seq[String]](jsonStr))
  }

  def testVector = {
    val jsonStr  = """["5XL","2XL","XL"]"""
    val expected = Vector("5XL", "2XL", "XL")
    assertEquals(Right(expected), parser.decode[Vector[String]](jsonStr))
  }

  def testSortedSet = {
    val jsonStr  = """["5XL","2XL","XL"]"""
    val expected = immutable.SortedSet("5XL", "2XL", "XL")
    assertEquals(Right(expected), parser.decode[immutable.SortedSet[String]](jsonStr))
  }

  def testHashSet = {
    val jsonStr  = """["5XL","2XL","XL"]"""
    val expected = immutable.HashSet("5XL", "2XL", "XL")
    assertEquals(Right(expected), parser.decode[immutable.HashSet[String]](jsonStr))
  }

  def testSet = {
    val jsonStr  = """["5XL","2XL","XL"]"""
    val expected = Set("5XL", "2XL", "XL")
    assertEquals(Right(expected), parser.decode[Set[String]](jsonStr))
  }

  def testMap = {
    val jsonStr  = """{"5XL":3,"2XL":14,"XL":159}"""
    val expected = Map("5XL" -> 3, "2XL" -> 14, "XL" -> 159)
    assertEquals(Right(expected), parser.decode[Map[String, Int]](jsonStr))
  }

  def testASTs =
    // we can't check that these are actually correct since they don't
    // roundtrip (due to number precision) and changing the ground truth data
    // would probably defeat the point of the tests.
    List(
      "jawn/bar.json",
      "jawn/bla25.json",
      "jawn/bla2.json",
      "jawn/countries.geo.json",
      "jawn/dkw-sample.json",
      "jawn/foo.json",
      "jawn/qux1.json",
      "jawn/qux2.json",
      "jawn/ugh10k.json"
    ).foreach { res =>
      val input = TestUtils.getResourceAsString(res)
      assert(parser.decode[JsValue](input).isRight, res)
    }

  def testStringy = {
    val expected = Stringy("wibble")
    assertEquals(Right(expected), parser.decode[Stringy](""""wibble""""))
  }

}
