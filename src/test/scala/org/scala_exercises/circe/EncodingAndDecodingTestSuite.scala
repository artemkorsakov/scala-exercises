package org.scala_exercises.circe

import io.circe._
import io.circe.generic.semiauto._
import io.circe.parser.decode
import io.circe.syntax._
import io.circe.generic.JsonCodec
import io.circe.generic.auto._
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

class EncodingAndDecodingTestSuite extends AnyFunSuiteLike with Matchers {
  test("test CIRCE LIB section Encoding And Decoding 0") {
    val intsJson = List(1, 2, 3).asJson
    println(intsJson)
    println(intsJson.as[List[Int]])
  }

  test("test CIRCE LIB section Encoding And Decoding 1") {
    val decodeList = decode[List[Int]]("[1, 2, 3]")
    decodeList.isRight should be(true)
    decodeList should be(Right(List(1, 2, 3)))
  }

  test("test CIRCE LIB section Encoding And Decoding 2") {
    case class Foo(a: Int, b: String, c: Boolean)
    implicit val fooDecoder: Decoder[Foo] = deriveDecoder[Foo]
    implicit val fooEncoder: Encoder[Foo] = deriveEncoder[Foo]
    // or Simply
    // implicit val fooDecoder: Decoder[Foo] = deriveDecoder
    // implicit val fooEncoder: Encoder[Foo] = deriveEncoder
  }

  test("test CIRCE LIB section Encoding And Decoding 3") {
    case class User(id: Long, firstName: String, lastName: String)

    object UserCodec {
      implicit val decodeUser: Decoder[User] =
        Decoder.forProduct3("id", "first_name", "last_name")(User.apply)

      implicit val encodeUser: Encoder[User] =
        Encoder.forProduct3("id", "first_name", "last_name")(u => (u.id, u.firstName, u.lastName))
    }
  }

  test("test CIRCE LIB section Encoding And Decoding 4") {
    case class Person(name: String)

    case class Greeting(salutation: String, person: Person, exclamationMarks: Int)

    val greetingJson = Greeting("Hey", Person("Chris"), 3).asJson

    greetingJson.hcursor.downField("person").downField("name").as[String] should be(Right("Chris"))
  }

}
