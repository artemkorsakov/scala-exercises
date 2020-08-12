package org.scala_exercises.scala_tutorial

import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

class DefinitionsAndEvaluationTestSuite extends AnyFunSuiteLike with Matchers {
  test("test Scala Tutorial LIB section DefinitionsAndEvaluation 0") {
    def square(x: Double) = x * x

    square(3.0) shouldBe 9.0
  }

  test("test Scala Tutorial LIB section DefinitionsAndEvaluation 1") {
    def square(x: Double) = x * x

    def area(radius: Double): Double = 3.14159 * square(radius)

    area(10) shouldBe 314.159
  }

  test("test Scala Tutorial LIB section DefinitionsAndEvaluation 2") {
    def triangleArea(base: Double, height: Double): Double = base * height / 2.0

    triangleArea(3, 4) shouldBe 6.0
    triangleArea(5, 6) shouldBe 15.0
  }

}
