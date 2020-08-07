package org.scala_exercises.std_lib

import org.scala_exercises.std_lib.testClasses._
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

object testClasses {
  class WithoutClassParameters {
    def addColors(red: Int, green: Int, blue: Int): (Int, Int, Int) =
      (red, green, blue)

    def addColorsWithDefaults(red: Int = 0, green: Int = 0, blue: Int = 0): (Int, Int, Int) =
      (red, green, blue)
  }

  class WithClassParameters(val defaultRed: Int, val defaultGreen: Int, val defaultBlue: Int) {
    def addColors(red: Int, green: Int, blue: Int): (Int, Int, Int) =
      (red + defaultRed, green + defaultGreen, blue + defaultBlue)

    def addColorsWithDefaults(red: Int = 0, green: Int = 0, blue: Int = 0): (Int, Int, Int) =
      (red + defaultRed, green + defaultGreen, blue + defaultBlue)
  }

  class WithClassParametersInClassDefinition(
      val defaultRed: Int = 0,
      val defaultGreen: Int = 255,
      val defaultBlue: Int = 100
  ) {
    def addColors(red: Int, green: Int, blue: Int): (Int, Int, Int) =
      (red + defaultRed, green + defaultGreen, blue + defaultBlue)

    def addColorsWithDefaults(red: Int = 0, green: Int = 0, blue: Int = 0): (Int, Int, Int) =
      (red + defaultRed, green + defaultGreen, blue + defaultBlue)
  }
}

class NamedAndDefaultArgumentsTestSuite extends AnyFunSuiteLike with Matchers {
  test("test STD LIB section NamedAndDefaultArguments 0") {
    val me = new WithoutClassParameters()

    // What happens if you change the order of these parameters? Nothing.
    val myColor = me.addColors(green = 0, red = 255, blue = 0)

    myColor should equal((255, 0, 0))
  }

  test("test STD LIB section NamedAndDefaultArguments 1") {
    val me      = new WithoutClassParameters()
    val myColor = me.addColorsWithDefaults(green = 255)

    myColor should equal((0, 255, 0))
  }

  test("test STD LIB section NamedAndDefaultArguments 2") {
    val me      = new WithClassParameters(40, 50, 60)
    val myColor = me.addColors(green = 50, red = 60, blue = 40)

    myColor should equal((100, 100, 100))
  }

  test("test STD LIB section NamedAndDefaultArguments 3") {
    val me      = new WithClassParameters(10, 20, 30)
    val myColor = me.addColorsWithDefaults(green = 70)

    myColor should equal((10, 90, 30))
  }

  test("test STD LIB section NamedAndDefaultArguments 4") {
    val me      = new WithClassParametersInClassDefinition()
    val myColor = me.addColorsWithDefaults(green = 70)

    myColor should equal((0, 325, 100))
  }

  test("test STD LIB section NamedAndDefaultArguments 5") {
    def reduce(a: Int, f: (Int, Int) => Int = _ + _): Int = f(a, a)

    reduce(5) should equal(10)
    reduce(5, _ * _) should equal(25)
  }

}
