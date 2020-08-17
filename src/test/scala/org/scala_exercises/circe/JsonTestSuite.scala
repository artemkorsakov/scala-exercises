package org.scala_exercises.circe

import circelib.helpers.JsonHelpers._
import io.circe.Json
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

class JsonTestSuite extends AnyFunSuiteLike with Matchers {
  test("test CIRCE LIB section Json 0") {
    jsonFromFields.noSpaces should be("{\"key1\":\"value1\",\"key2\":1}")
  }

  test("test CIRCE LIB section Json 1") {
    "{\"key\":\"value\"}" should be(Json.fromFields(List(("key", Json.fromString("value")))).noSpaces)

    "{\"name\":\"sample json\",\"data\":{\"done\":false}}" should be(
      Json
        .fromFields(
          List(
            ("name", Json.fromString("sample json")),
            ("data", Json.fromFields(List(("done", Json.fromBoolean(false)))))
          )
        )
        .noSpaces
    )

    "[{\"x\":1}]" should be(Json.fromValues(List(Json.fromFields(List(("x", Json.fromInt(1)))))).noSpaces)
  }

  test("test CIRCE LIB section Json 2") {
    transformJson(jsonArray).noSpaces should be("[{\"field1\":1}]")
  }

}
