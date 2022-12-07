package jzon.internal

class StringMatrixTest extends Test {

  def fail(msg: String): Nothing = throw new AssertionError(msg)

  def matcher(xs: List[String], test: String): List[String] = {
    val m = new StringMatrix(xs.toArray)
    var bs = test.zipWithIndex.foldLeft(m.initial) {
      case (bs, (c, i)) => m.update(bs, i, c)
    }
    bs = m.exact(bs, test.length)
    matches(xs, bs)
  }

  def matches(xs: List[String], bitset: Long): List[String] = {
    var hits: List[String] = Nil
    var i                  = 0
    while (i < xs.length) {
      if (((bitset >>> i) & 1L) == 1L)
        hits = xs(i) :: hits
      i += 1
    }
    hits
  }

  val genStrings: Gen[List[String]] = Gen.nel(Gen.alphanumeric(), 63)

  def testPositiveSucceeds = Gen.prop(genStrings) { xs =>
    xs.foreach(s => matcher(xs, s).contains(s) || fail(xs.toString))
  }

  def testNegativeFails = Gen.prop(genStrings) { xs =>
    xs.exists(_.startsWith("wibble")) ||
    matcher(xs, "wibble") == Nil ||
    fail(xs.toString)
  }

  def testSubstringFails = Gen.prop(genStrings) { xs =>
    xs.length < 2 ||
    matcher(xs, xs.mkString) == Nil ||
    fail(xs.toString)
  }

  def testTrivial = Gen.prop(Gen.alphanumeric()) { s =>
    matcher(List(s), s) == List(s) ||
    fail(s)
  }

  def testExactMatchIsASubstring =
    assertEquals(
      List("retweeted"),
      matcher(List("retweeted_status", "retweeted"), "retweeted")
    )

}
