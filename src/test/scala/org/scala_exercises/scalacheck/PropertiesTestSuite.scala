package org.scala_exercises.scalacheck

import org.scalacheck.Gen
import org.scalacheck.Prop.{ forAll, propBoolean }
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.Checkers.check

class PropertiesTestSuite extends AnyFunSuiteLike with Matchers {
  test("test SCALACHECK LIB section Properties 0") {
    val propConcatLists = forAll((l1: List[Int], l2: List[Int]) => l1.size + l2.size == (l1 ::: l2).size)
    println(propConcatLists.check)

    val propSqrt = forAll((n: Int) => scala.math.sqrt(n * n) == n)
    println(propSqrt.check)
  }

  test("test SCALACHECK LIB section Properties 1") {
    check {
      forAll((s1: String, s2: String) => (s1 + s2).endsWith(s2) == true)
    }
  }

  test("test SCALACHECK LIB section Properties 2") {

    val smallInteger = Gen.choose(0, 100)

    check {
      forAll(smallInteger)(n => (n >= 0 && n <= 100) == true)
    }
  }

  test("test SCALACHECK LIB section Properties 3") {
    check {
      forAll { n: Int => (n % 2 == 0) ==> (n % 2 == 0) }
    }
  }

  test("test SCALACHECK LIB section Properties 4") {
    val smallInteger = Gen.choose(0, 100)

    check {
      forAll(smallInteger) { n =>
        (n > 100) == false
      } &&
      forAll(smallInteger)(n => (n >= 0) == true)
    }
  }

  test("test SCALACHECK LIB section Properties 5") {
    import org.scalacheck.{ Prop, Properties }

    class ZeroSpecification extends Properties("Zero") {

      import org.scalacheck.Prop.{ forAll, propBoolean }

      property("addition property") = forAll { n: Int => (n != 0) ==> (n + 0 == n) }
      property("additive inverse property") = forAll { n: Int => (n != 0) ==> (n + (-n) == 0) }
      property("multiplication property") = forAll { n: Int => (n != 0) ==> (n * 0 == 0) }
    }

    check(Prop.all(new ZeroSpecification().properties.to(List).map(_._2): _*))
  }

}
