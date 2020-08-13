package org.scala_exercises.shapeless

import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers
import shapeless.test.illTyped

import scala.util.Try

class TypeCheckingTestSuite extends AnyFunSuiteLike with Matchers {
  test("test SHAPELESS LIB section Type Checking 0") {
    println(illTyped("""1+1 : Boolean"""))

    //println(illTyped("""1+1 : Int"""))
  }

  test("test SHAPELESS LIB section Type Checking 1") {
    val matchedTypes = Try(assertTypeError("illTyped { \"val a: Int = 1\" }")).isSuccess
    matchedTypes should be(true)

    val mismatchedTypes = Try(assertTypeError("illTyped { \"val a: String = 1\" }")).isSuccess
    mismatchedTypes should be(false)
  }

}
