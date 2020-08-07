package org.scala_exercises.std_lib

import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

class UniformAccessPrincipleTestSuite extends AnyFunSuiteLike with Matchers {
  test("test STD LIB section UniformAccessPrinciple 0") {
    class Test1(val age: Int = 10)
    class Test2(_age: Int) {
      def age: Int = _age
    }

    new Test1(10).age should be(10)
    new Test2(11).age should be(11)
  }

}
