package org.scala_exercises.scalacheck

import org.scalacheck.Gen
import org.scalacheck.Gen.{ alphaChar, listOfN, posNum }
import org.scalacheck.Prop.forAll
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.Checkers._

class GeneratorsTestSuite extends AnyFunSuiteLike with Matchers {
  test("test SCALACHECK LIB section Generators 0") {

    val myGen = for {
      n <- Gen.choose(10, 20)
      m <- Gen.choose(2 * n, 500)
    } yield (n, m)

    check {
      forAll(myGen) {
        case (n, m) => (m >= 2 * n) == true
      }
    }
  }

  test("test SCALACHECK LIB section Generators 1") {
    val vowel = Gen.oneOf('A', 'E', 'I', 'O', 'U')

    val validChars: Seq[Char] = Seq('A', 'E', 'I', 'O', 'U')

    check {
      forAll(vowel)(v => validChars.contains(v))
    }
  }

  test("test SCALACHECK LIB section Generators 2") {
    check {
      forAll(alphaChar)(_.isDigit == false)
    }

    check {
      forAll(posNum[Int])(n => (n > 0) == true)
    }

    check {
      forAll(listOfN(10, posNum[Int]))(list => !list.exists(_ < 0) && list.length == 10)
    }
  }

  test("test SCALACHECK LIB section Generators 3") {
    val smallEvenInteger = Gen.choose(0, 200) suchThat (_ % 2 == 0)

    check {
      forAll(smallEvenInteger)(_ % 2 == 0)
    }
  }

  test("test SCALACHECK LIB section Generators 4") {
    case class Foo(intValue: Int, charValue: Char)

    val fooGen = for {
      intValue  <- Gen.posNum[Int]
      charValue <- Gen.alphaChar
    } yield Foo(intValue, charValue)

    check {
      forAll(fooGen)(foo => foo.intValue > 0 && foo.charValue.isDigit == false)
    }
  }

  test("test SCALACHECK LIB section Generators 5") {
    val myGen = Gen.sized { size =>
      val positiveNumbers = size / 3
      val negativeNumbers = size * 2 / 3
      for {
        posNumList <- Gen.listOfN(positiveNumbers, Gen.posNum[Int])
        negNumList <- Gen.listOfN(negativeNumbers, Gen.posNum[Int] map (n => -n))
      } yield (size, posNumList, negNumList)
    }

    check {
      forAll(myGen) {
        case (genSize, posN, negN) =>
          posN.length == genSize / 3 && negN.length == genSize * 2 / 3
      }
    }
  }

  test("test SCALACHECK LIB section Generators 6") {
    val genIntList = Gen.containerOf[List, Int](Gen.oneOf(2, 4, 6))

    val validNumbers: List[Int] = List(2, 4, 6)

    check {
      forAll(genIntList)(_ forall (elem => validNumbers.contains(elem)))
    }
  }

}
