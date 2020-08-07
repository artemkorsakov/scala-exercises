package org.scala_exercises.std_lib

import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

class ByNameParameterTestSuite extends AnyFunSuiteLike with Matchers {
  test("test STD LIB section ByNameParameter 0") {
    def calc(x: () => Int): Either[Throwable, Int] =
      try Right(x()) // An explicit call of the x function
      catch {
        case b: Throwable => Left(b)
      }

    val y = calc { () => // explicitly declaring that Unit is a parameter with ()
      14 + 15
    }

    y should be(Right(29))
  }

  test("test STD LIB section ByNameParameter 1") {
    def calc(x: => Int): Either[Throwable, Int] =
      // x is a call by-name parameter
      try Right(x)
      catch {
        case b: Throwable => Left(b)
      }

    val y = calc {
      // This looks like a natural block
      println("Here we go!") // Some superfluous call
      val z = List(1, 2, 3, 4) // Another superfluous call
      49 + 20
    }

    y should be(Right(69))
  }

  test("test STD LIB section ByNameParameter 2") {
    object PigLatinizer {
      def apply(x: => String): String = x.tail + x.head + "ay"
    }

    val result = PigLatinizer {
      val x = "pret"
      val z = "zel"
      x ++ z //concatenate the strings
    }

    result should be("retzelpay")
  }

}
