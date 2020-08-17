package org.scala_exercises.circe

import circelib.helpers.CustomCodecsHelpers.json
import io.circe._
import io.circe.generic.extras.{ Configuration, JsonKey }
import io.circe.syntax._
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

class CustomCodecsTestSuite extends AnyFunSuiteLike with Matchers {
  test("test CIRCE LIB section Custom Codecs 0") {
    json.hcursor.downField("hello").as[Int] should be(Right(123))
  }

  test("test CIRCE LIB section Custom Codecs 1") {
    case class User(firstName: String, lastName: String)

    implicit val encodeFoo: Encoder[User] = (a: User) =>
      Json
        .fromFields(
          List(
            ("first_name", Json.fromString(a.firstName)),
            ("last_name", Json.fromString(a.lastName))
          )
        )

    implicit val decodeFoo: Decoder[User] = (c: HCursor) => Left(DecodingFailure("Not implemented yet", c.history))

    User("Foo", "McBar").asJson.noSpaces shouldBe """{"first_name":"Foo","last_name":"McBar"}"""
  }

  test("test CIRCE LIB section Custom Codecs 2") {
    val config: Configuration = Configuration.default.copy(transformMemberNames = {
      case "i"   => "my-int"
      case other => other
    })

    case class Bar(i: Int, s: String)

    implicit val encodeFoo: Encoder[Bar] = (a: Bar) =>
      Json
        .fromFields(
          List(
            ("my-int", Json.fromInt(a.i)),
            ("s", Json.fromString(a.s))
          )
        )

    Bar(13, "Qux").asJson.noSpaces shouldBe """{"my-int":13,"s":"Qux"}"""
  }

  test("test CIRCE LIB section Custom Codecs 3") {
    val config: Configuration = Configuration.default

    case class Bar(@JsonKey("my-int") i: Int, s: String)

    implicit val encodeFoo: Encoder[Bar] = (a: Bar) =>
      Json
        .fromFields(
          List(
            ("my-int", Json.fromInt(a.i)),
            ("s", Json.fromString(a.s))
          )
        )

    Bar(13, "Qux").asJson.noSpaces shouldBe """{"my-int":13,"s":"Qux"}"""

  }

}
