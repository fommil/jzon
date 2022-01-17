package zio.json.compat

import zio.json._
import zio.json.internal._
import zio.json.syntax._

import _root_.scalaz._
import zio.json.compat.scalaz._

class ScalazTest extends Test {

  def testScalaz = {
    assertEquals("[]", IList[Int]().toJson)
    assertEquals("[1,2,3]", IList(1, 2, 3).toJson)

    assertEquals("[]", IList[Int]().toJsonPretty)
    assertEquals("[1, 2, 3]", IList(1, 2, 3).toJsonPretty)

    assertEquals(
      Right(IList(1, 2, 3)),
      parser.decode[IList[Int]]("""[1,2,3]""")
    )
  }

}
