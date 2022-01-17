package zio.json.internal

class SafeNumbersTest extends Test {

  def testValidBigDecimal = Gen.prop(Gen.bigdecimal(2048)) { i =>
    assertEquals(Some(i), SafeNumbers.bigdecimal(i.toString, 2048))
  }

  def testValidBigDecimalEdgeCases =
    List(
      ".0",
      "-.0",
      "0",
      "0.0",
      "-0.0", // zeroes
      "0000.1",
      "0.00001",
      "000.00001000" // various trailing zeros, should be preserved
    ).foreach(s => assertEquals(Some(new java.math.BigDecimal(s)), SafeNumbers.bigdecimal(s, 2048)))

  def testInvalidBigDecimal = Gen.prop(Gen.alpha())(s => assertEquals(None, SafeNumbers.bigdecimal(s)))

  def testInvalidBigDecimalEdgeCases =
    List(
      "N",
      "Inf",
      "-NaN",
      "+NaN",
      "e1",
      "1.1.1",
      "1 ",
      "NaN",
      "Infinity",
      "+Infinity",
      "-Infinity"
    ).foreach(s => assertEquals(None, SafeNumbers.bigdecimal(s)))

  def testValidBigInteger = Gen.prop(Gen.biginteger(2048)) { i =>
    assertEquals(Some(i), SafeNumbers.biginteger(i.toString, 2048))
  }

  def testValidBigIntegerEdgeCases =
    List(
      "00",
      "01",
      "0000001",
      "-9223372036854775807",
      "9223372036854775806",
      "-9223372036854775809",
      "9223372036854775808"
    ).foreach(s => assertEquals(Some(new java.math.BigInteger(s)), SafeNumbers.biginteger(s)))

  def testInvalidBigInteger = Gen.prop(Gen.alpha())(s => assertEquals(None, SafeNumbers.biginteger(s)))

  def testInvalidBigIntegerEdgeCases =
    List("0foo", "01foo", "0.1", "", "1 ").foreach(s => assertEquals(None, SafeNumbers.biginteger(s)))

  // byte
  def testValidByte = Gen.prop(Gen.byte)(i => assertEquals(ByteSome(i), SafeNumbers.byte(i.toString)))

  def testInvalidByte = Gen.prop(Gen.alpha())(s => assertEquals(ByteNone, SafeNumbers.byte(s)))

  // short
  def testValidShort = Gen.prop(Gen.short)(i => assertEquals(ShortSome(i), SafeNumbers.short(i.toString)))

  def testInvalidShort = Gen.prop(Gen.alpha())(s => assertEquals(ShortNone, SafeNumbers.short(s)))

  // int
  def testValidInt = Gen.prop(Gen.int)(i => assertEquals(IntSome(i), SafeNumbers.int(i.toString)))

  def testInvalidInt = Gen.prop(Gen.alpha())(s => assertEquals(IntNone, SafeNumbers.int(s)))

  // long
  def testValidLong = Gen.prop(Gen.long)(i => assertEquals(LongSome(i), SafeNumbers.long(i.toString)))

  def testValidLongEdgeCases =
    List("00", "01", "0000001", "-9223372036854775807", "9223372036854775806")
      .foreach(s => assertEquals(LongSome(s.toLong), SafeNumbers.long(s)))

  def testInvalidLong = Gen.prop(Gen.alpha())(s => assertEquals(LongNone, SafeNumbers.long(s)))

  def testInvalidLongEdgeCases =
    List(
      "0foo",
      "01foo",
      "0.1",
      "",
      "1 ",
      "-9223372036854775809",
      "9223372036854775808"
    ).foreach(i => assertEquals(LongNone, SafeNumbers.long(i)))

  // float
  def testValidFloat = Gen.prop(Gen.float) { i =>
    if (java.lang.Float.isFinite(i))
      assertEquals(FloatSome(i), SafeNumbers.float(i.toString))
  }

  def testValidFloatFromInt = Gen.prop(Gen.int) { i =>
    assertEquals(FloatSome(i.toFloat), SafeNumbers.float(i.toString))
  }

  def testValidFloatFromDouble = Gen.prop(Gen.double) { i =>
    if (java.lang.Double.isFinite(i))
      assertEquals(FloatSome(i.toFloat), SafeNumbers.float(i.toString))
  }

  def testValidFloatEdgeCases =
    List(
      ".0",
      "-.0",
      "0",
      "0.0",
      "-0.0", // zeroes
      "0000.1",
      "0.00001",
      "000.00001000", // trailing zeros
      "NaN",
      "92233720368547758070", // overflows a Long significand
      "Infinity",
      "+Infinity",
      "-Infinity",
      "1.199999988079071" // large mantissa
    ).foreach { s =>
      // do the comparison on strings to deal with NaNs
      assertEquals(FloatSome(s.toFloat).toString, SafeNumbers.float(s).toString)
    }

  def testInvalidFloat = Gen.prop(Gen.alpha()) { s =>
    if (s != "NaN" && s != "Infinity")
      assertEquals(FloatNone, SafeNumbers.float(s))
  }

  // note that in a stream, 1.1.1 may parse "1.1" leaving ".1"
  def testInvalidFloatEdgeCases =
    List("N", "Inf", "-NaN", "+NaN", "e1", "1.1.1")
      .foreach(s => assertEquals(FloatNone, SafeNumbers.float(s)))

  // double
  def testValidDouble = Gen.prop(Gen.double) { i =>
    if (java.lang.Double.isFinite(i))
      assertEquals(DoubleSome(i), SafeNumbers.double(i.toString))
  }

  def testValidDoubleFromLong = Gen.prop(Gen.long) { i =>
    assertEquals(DoubleSome(i.toDouble), SafeNumbers.double(i.toString))
  }

  def validDoubleEdgeCases =
    List(
      ".0",
      "-.0",
      "0",
      "0.0",
      "-0.0", // zeroes
      "0000.1",
      "0.00001",
      "000.00001000", // trailing zeros
      "NaN",
      "92233720368547758070", // overflows a Long significand
      "Infinity",
      "+Infinity",
      "-Infinity",
      "3.976210887433566E-281" // rounds if a naive scaling is used
    ).foreach { s =>
      // do the comparison on strings to deal with NaNs
      assertEquals(DoubleSome(s.toDouble).toString, SafeNumbers.double(s).toString)
    }

  def testInvalidDouble = Gen.prop(Gen.alpha()) { s =>
    if (s != "NaN" && s != "Infinity")
      assertEquals(DoubleNone, SafeNumbers.double(s))
  }

  def testInvalidDoubleEdgeCases =
    List("N", "Inf", "-NaN", "+NaN", "e1", "1.1.1", "1 ")
      .foreach(s => assertEquals(DoubleNone, SafeNumbers.double(s)))

}
