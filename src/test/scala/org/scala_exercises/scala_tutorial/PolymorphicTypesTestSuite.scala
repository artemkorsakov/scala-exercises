package org.scala_exercises.scala_tutorial

import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

class PolymorphicTypesTestSuite extends AnyFunSuiteLike with Matchers {
  test("test Scala Tutorial LIB section PolymorphicTypes 0") {
    def size[A](xs: List[A]): Int =
      xs match {
        case Nil     => 0
        case y :: ys => 1 + size(ys)
      }
    size(Nil) shouldBe 0
    size(List(1, 2)) shouldBe 2
    size(List("a", "b", "c")) shouldBe 3
  }

}
