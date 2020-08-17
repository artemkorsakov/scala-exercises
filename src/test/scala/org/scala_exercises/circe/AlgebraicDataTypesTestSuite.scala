package org.scala_exercises.circe

import circelib.helpers.ADTHelpers._
import io.circe.syntax._
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

class AlgebraicDataTypesTestSuite extends AnyFunSuiteLike with Matchers {
  test("test CIRCE LIB section ADT (Algebraic Data Types) 0") {
    import circelib.helpers.ADTHelpers.GenericDerivation._
    import io.circe.parser.decode

    decode[Event]("{ \"i\": 1000 }") shouldBe Right(Foo(1000))
    (Foo(100): Event).asJson.noSpaces shouldBe "{\"i\":100}"
  }

  test("test CIRCE LIB section ADT (Algebraic Data Types) 1") {
    import circelib.helpers.ADTHelpers.ShapesDerivation._
    import io.circe.generic.auto._
    import io.circe.shapes._
    import io.circe.parser.decode

    decode[Event]("{ \"i\": 1000 }") shouldBe Right(Foo(1000))
    (Foo(100): Event).asJson.noSpaces shouldBe "{\"i\":100}"
  }

  test("test CIRCE LIB section ADT (Algebraic Data Types) 2") {
    import circelib.helpers.ADTHelpers.GenericExtraDerivation._
    import io.circe.parser.decode
    import io.circe.generic.extras.auto._

    decode[Event]("{ \"i\": 1000, \"what_am_i\": \"Foo\" }") shouldBe Right(Foo(1000))
  }

}
