package org.scala_exercises.std_lib

import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

import scala.language.postfixOps

class InfixPrefixAndPostfixOperatorsTestSuite extends AnyFunSuiteLike with Matchers {
  test("test STD LIB section InfixPrefixAndPostfixOperators 0") {
    // Any method which takes a single parameter can be used as an infix operator: a.m(b) can also be written as a m b.

    val g: Int = 3
    (g + 4) should be(7) // + is an infix operator
    g.+(4) should be(7)
  }

  test("test STD LIB section InfixPrefixAndPostfixOperators 1") {
    // Infix operators do NOT work if an object has a method that takes two parameters:

    val g: String = "Check out the big brains on Brad!"

    g indexOf 'o' should be(6) //indexOf(Char) can be used as an infix operator

    // g indexOf 'o', 4 should be (6) //indexOf(Char, Int) cannot be used as an infix operator

    g.indexOf('o', 7) should be(25)
  }

  test("test STD LIB section InfixPrefixAndPostfixOperators 2") {
    // Any method which does not require a parameter can be used as a postfix operator: a.m can be written as a m.
    // For instance, a.+(b) is equivalent to a + b and a.! is the same as a!.
    // Postfix operators have lower precedence than infix operators, so:
    // foo bar baz means foo.bar(baz).
    // foo bar baz bam means (foo.bar(baz)).bam
    // foo bar baz bam bim means (foo.bar(baz)).bam(bim).

    val g: Int = 31
    (g toHexString) should be("1f")
  }

  test("test STD LIB section InfixPrefixAndPostfixOperators 3") {
    // Prefix operators work if an object has a method name that starts with unary_ :

    val g: Int = 31
    (-g) should be(-31)
  }

  test("test STD LIB section InfixPrefixAndPostfixOperators 4") {
    // Here's how to create a prefix operator for our own class. The only identifiers that can be used as prefix operators are +, -, !, and ~:

    class Stereo {
      def unary_+ = "on"

      def unary_- = "off"
    }

    val stereo = new Stereo
    (+stereo) should be("on")
    (-stereo) should be("off")
  }

}
