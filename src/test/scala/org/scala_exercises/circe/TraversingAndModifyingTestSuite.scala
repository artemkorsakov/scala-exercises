package org.scala_exercises.circe

import io.circe.Decoder.Result
import io.circe._
import io.circe.parser._
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

class TraversingAndModifyingTestSuite extends AnyFunSuiteLike with Matchers {
  test("test CIRCE LIB section Traversing And Modifying 0") {
    val json: String = """
      {
        "id": "c730433b-082c-4984-9d66-855c243266f0",
        "name": "Foo",
        "counts": [1, 2, 3],
        "values": {
          "bar": true,
          "baz": 100.001,
          "qux": ["a", "b"]
        }
      } """

    val doc: Json                   = parse(json).getOrElse(Json.Null)
    val cursor: HCursor             = doc.hcursor
    val baz: Decoder.Result[Double] = cursor.downField("values").downField("baz").as[Double]
    baz should be(Right(100.001))

    val baz2: Decoder.Result[Double] = cursor.downField("values").get[Double]("baz")
    baz2 should be(Right(100.001))

    val secondQux: Decoder.Result[String] =
      cursor.downField("values").downField("qux").downArray.right.as[String]
    secondQux should be(Right("b"))

    val reversedNameCursor: ACursor =
      cursor.downField("name").withFocus(_.mapString(_.reverse))

    val reversedName: Option[Json] = reversedNameCursor.top

    val nameResult: Result[String] =
      cursor.downField("name").withFocus(_.mapString(_.reverse)).as[String]

    nameResult should be(Right("ooF"))
  }

}
