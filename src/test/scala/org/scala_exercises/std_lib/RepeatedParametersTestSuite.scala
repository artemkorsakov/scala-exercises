package org.scala_exercises.std_lib

import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

class RepeatedParametersTestSuite extends AnyFunSuiteLike with Matchers {
  test("test STD LIB section RepeatedParameters 0") {
    def repeatedParameterMethod(x: Int, y: String, z: Any*) =
      "%d %ss can give you %s".format(x, y, z.mkString(", "))

    repeatedParameterMethod(3, "egg", "a delicious sandwich", "protein", "high cholesterol") should be(
      "3 eggs can give you a delicious sandwich, protein, high cholesterol"
    )

    repeatedParameterMethod(3, "egg", List("a delicious sandwich", "protein", "high cholesterol")) should be(
      "3 eggs can give you List(a delicious sandwich, protein, high cholesterol)"
    )

    repeatedParameterMethod(3, "egg", List("a delicious sandwich", "protein", "high cholesterol"): _*) should be(
      "3 eggs can give you a delicious sandwich, protein, high cholesterol"
    )
  }

}
