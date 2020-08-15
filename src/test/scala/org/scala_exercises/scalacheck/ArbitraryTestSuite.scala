package org.scala_exercises.scalacheck

import org.scalacheck.{ Arbitrary, Gen }
import org.scalacheck.Gen.listOfN
import org.scalacheck.Prop.forAll
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.Checkers._

class ArbitraryTestSuite extends AnyFunSuiteLike with Matchers {
  test("test SCALACHECK LIB section Arbitrary 0") {
    implicit lazy val myCharArbitrary = Arbitrary(Gen.oneOf('A', 'E', 'I', 'O', 'U'))

    val validChars: Seq[Char] = Seq('A', 'E', 'I', 'O', 'U')

    check(forAll { c: Char => validChars.contains(c) })
  }

  test("test SCALACHECK LIB section Arbitrary 1") {
    case class Foo(intValue: Int, charValue: Char)

    val fooGen = for {
      intValue  <- Gen.posNum[Int]
      charValue <- Gen.alphaChar
    } yield Foo(intValue, charValue)

    implicit lazy val myFooArbitrary = Arbitrary(fooGen)

    check(forAll { foo: Foo => (foo.intValue < 0) == false && !foo.charValue.isDigit })
  }

  test("test SCALACHECK LIB section Arbitrary 2") {
    val genEightBytes = listOfN(8, Arbitrary.arbitrary[Byte])

    check(forAll(genEightBytes)(list => list.size == 8))
  }

}
