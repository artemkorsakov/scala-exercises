package org.scala_exercises.std_lib

import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

class ImplicitsTestSuite extends AnyFunSuiteLike with Matchers {
  test("test STD LIB section Implicits 0") {
    abstract class SemiGroup[A] {
      def add(x: A, y: A): A
    }
    abstract class Monoid[A] extends SemiGroup[A] {
      def unit: A
    }
    object ImplicitTest extends App {
      implicit object StringMonoid extends Monoid[String] {
        def add(x: String, y: String): String = x concat y
        def unit: String                      = ""
      }
      implicit object IntMonoid extends Monoid[Int] {
        def add(x: Int, y: Int): Int = x + y
        def unit: Int                = 0
      }
      def sum[A](xs: List[A])(implicit m: Monoid[A]): A =
        if (xs.isEmpty) m.unit
        else m.add(xs.head, sum(xs.tail))
    }

    import ImplicitTest._

    println(sum(List(1, 2, 3)))
    println(sum(List("a", "b", "c")))
  }

  test("test STD LIB section Implicits 1") {
    // Implicits wrap around existing classes to provide extra functionality.
    // This is similar to monkey patching in Ruby and meta-programming in Groovy.
    // Creating a method isOdd for Int, which doesn't exist:
    class KoanIntWrapper(val original: Int) {
      def isOdd = original % 2 != 0
    }

    implicit def thisMethodNameIsIrrelevant(value: Int) =
      new KoanIntWrapper(value)

    19.isOdd should be(true)
    20.isOdd should be(false)
  }

  test("test STD LIB section Implicits 2") {
    // Implicits rules can be imported into your scope with an import:
    object MyPredef {

      class KoanIntWrapper(val original: Int) {
        def isOdd = original % 2 != 0

        def isEven = !isOdd
      }

      implicit def thisMethodNameIsIrrelevant(value: Int) =
        new KoanIntWrapper(value)
    }

    import MyPredef._
    //imported implicits come into effect within this scope
    19.isOdd should be(true)
    20.isOdd should be(false)
  }

  test("test STD LIB section Implicits 3") {
    // Implicits can be used to automatically convert a value's type to another:
    import java.math.BigInteger
    implicit def Int2BigIntegerConvert(value: Int): BigInteger =
      new BigInteger(value.toString)

    def add(a: BigInteger, b: BigInteger) = a.add(b)

    add(Int2BigIntegerConvert(3), Int2BigIntegerConvert(6)) == Int2BigIntegerConvert(9) should be(true)

    add(3, 6) == 9 should be(false)
    add(3, 6) == Int2BigIntegerConvert(9) should be(true)

    add(3, 6) == (9: BigInteger) should be(true)
    add(3, 6).intValue == 9 should be(true)
  }

  test("test STD LIB section Implicits 4") {
    // Implicits can be used to declare a value to be provided as a default as long as an implicit value is set with in the scope.
    // These are called Implicit Function Parameters:

    def howMuchCanIMake_?(hours: Int)(implicit dollarsPerHour: BigDecimal) =
      dollarsPerHour * hours

    implicit val hourlyRate = BigDecimal(34)

    howMuchCanIMake_?(30) should be(1020)
  }

  test("test STD LIB section Implicits 5") {
    // Implicit Function Parameters can contain a list of implicits:

    def howMuchCanIMake_?(hours: Int)(implicit amount: BigDecimal, currencyName: String) =
      (amount * hours).toString() + " " + currencyName

    implicit val hourlyRate   = BigDecimal(34)
    implicit val currencyName = "Dollars"

    howMuchCanIMake_?(30) should be("1020 Dollars")
  }

  test("test STD LIB section Implicits 6") {
    // Default arguments, though, are preferred to Implicit Function Parameters:
    def howMuchCanIMake_?(hours: Int, amount: BigDecimal = 34, currencyName: String = "Dollars") =
      (amount * hours).toString() + " " + currencyName

    howMuchCanIMake_?(30) should be("1020 Dollars")

    howMuchCanIMake_?(30, 95) should be("2850 Dollars")
  }

}
