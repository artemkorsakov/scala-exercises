package org.scala_exercises.circe

import io.circe._
import io.circe.generic.semiauto._
import io.circe.parser.decode
import io.circe.syntax._
import io.circe.generic.JsonCodec
import BarHelper._
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

object BarHelper {
  @JsonCodec case class Bar(i: Int, s: String)
}

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
    val bar = Bar(13, "Qux").asJson
    // res4: io.circe.Json =
    // {
    //   "i" : 13,
    //   "s" : "Qux"
    // }

    println(bar)
  }

  test("test CIRCE LIB section Encoding And Decoding 4") {}

  test("test CIRCE LIB section Encoding And Decoding 5") {}

  test("test CIRCE LIB section Encoding And Decoding 6") {}

  test("test CIRCE LIB section Encoding And Decoding 7") {}

  test("test CIRCE LIB section Encoding And Decoding 8") {}

  test("test CIRCE LIB section Encoding And Decoding 9") {}

  test("test CIRCE LIB section Encoding And Decoding 10") {}

  test("test CIRCE LIB section Encoding And Decoding 11") {}

  test("test CIRCE LIB section Encoding And Decoding 12") {}

  test("test CIRCE LIB section Encoding And Decoding 13") {}

  test("test CIRCE LIB section Encoding And Decoding 14") {}

  test("test CIRCE LIB section Encoding And Decoding 15") {}

  test("test CIRCE LIB section Encoding And Decoding 16") {}

  test("test CIRCE LIB section Encoding And Decoding 17") {}

  test("test CIRCE LIB section Encoding And Decoding 18") {}

  test("test CIRCE LIB section Encoding And Decoding 19") {}

  test("test CIRCE LIB section Encoding And Decoding 20") {}

}
