package org.scala_exercises.std_lib

import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

class TypeSignaturesTestSuite extends AnyFunSuiteLike with Matchers {
  test("test STD LIB section TypeSignatures 0") {
    trait Randomizer[A] {
      def draw(): A
    }

    class IntRandomizer extends Randomizer[Int] {
      def draw() = {
        import util.Random
        Random.nextInt()
      }
    }

    val intRand = new IntRandomizer
    (intRand.draw <= Int.MaxValue) should be(true)
  }

  test("test STD LIB section TypeSignatures 1") {
    classOf[String].getCanonicalName should be("java.lang.String")
    classOf[String].getSimpleName should be("String")
  }

  test("test STD LIB section TypeSignatures 2") {
    val zoom = "zoom"
    zoom.isInstanceOf[String] should be(true)
    zoom.getClass.getCanonicalName should be("java.lang.String")
    zoom.getClass.getSimpleName should be("String")
  }

  test("test STD LIB section TypeSignatures 3") {
    trait Randomizer[A] {
      def draw(): A
    }

    class IntRandomizer extends Randomizer[Int] {
      def draw(): Int = {
        import util.Random
        Random.nextInt()
      }
    }

    val intRand = new IntRandomizer
    intRand.isInstanceOf[IntRandomizer] should be(true)
    intRand.isInstanceOf[Randomizer[Int]] should be(true)
    intRand.draw.isInstanceOf[Int] should be(true)
  }

}
