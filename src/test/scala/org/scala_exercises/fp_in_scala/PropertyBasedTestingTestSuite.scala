package org.scala_exercises.fp_in_scala

import fpinscalalib.customlib.state.{ RNG, State }
import fpinscalalib.customlib.testing.{ Gen, SGen }
import fpinscalalib.customlib.testing.Gen._
import fpinscalalib.customlib.testing.Prop._
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

class PropertyBasedTestingTestSuite extends AnyFunSuiteLike with Matchers {
  test("test FP IN SCALA LIB section Property Based Testing 0") {
    val genString = Gen.stringN(5)

    val propA = forAll(genString)(s => s.substring(4) == s.charAt(4).toString)
    val propB = forAll(genString)(s => s.startsWith(s) == true)

    // Don't worry about the implementations of run and its types, we'll deal with it later:
    val result = (propA && propB).run(100, 100, RNG.Simple(System.currentTimeMillis))
    result shouldBe Passed
  }

  test("test FP IN SCALA LIB section Property Based Testing 1") {
    def choose(start: Int, stopExclusive: Int): Gen[Int] =
      Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))

    val rng = RNG.Simple(47)
    // We use sample on the Gen instance to generate a State containing a RNG and an Int value. We can then run it and
    // obtain both the random-generated number and a new generator to keep generating more.
    choose(5, 10).sample.run(rng)._1 should be >= 5

    choose(0, 6).sample.run(rng)._1 should be < 6
  }

  test("test FP IN SCALA LIB section Property Based Testing 2") {
    def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

    val rng = RNG.Simple(47)
    unit(42).sample.run(rng)._1 shouldBe 42

    unit("foo").sample.run(rng)._1 shouldBe "foo"
  }

  test("test FP IN SCALA LIB section Property Based Testing 3") {
    def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
      Gen(State.sequence(List.fill(n)(g.sample)))

    val rng            = RNG.Simple(47)
    val listOfBooleans = listOfN(10, Gen.boolean).sample.run(rng)._1
    listOfBooleans.size shouldBe 10

    listOfN(3, Gen.unit(42)).sample.run(rng)._1 shouldBe List(42, 42, 42)
  }

  test("test FP IN SCALA LIB section Property Based Testing 4") {
    def listOfN_1[A](size: Gen[Int], g: Gen[A]): Gen[List[A]] =
      size flatMap (n => Gen.listOfN(n, g))

    val rng           = RNG.Simple(47)
    val intGen        = choose(0, 10)
    val generatedList = listOfN_1(intGen, unit(42)).sample.run(rng)._1
    generatedList.size should be >= 0

    generatedList.size should be < 10

    listOfN_1(Gen.unit(1), Gen.unit(42)).sample.run(rng)._1 shouldBe List(42)
  }

  test("test FP IN SCALA LIB section Property Based Testing 5") {
    val genZeroToTen      = Gen.choose(0, 10)
    val genElevenToTwenty = Gen.choose(11, 20)
    val genCombination    = Gen.union(genZeroToTen, genElevenToTwenty)

    val combinedProp = (forAll(genCombination)(_ < 10) ||
      forAll(genCombination)(_ < 20)) &&
      forAll(genCombination)(_ >= 0)

    val result = combinedProp.run(100, 100, RNG.Simple(System.currentTimeMillis))
    result shouldBe Passed
  }

  test("test FP IN SCALA LIB section Property Based Testing 6") {
    def listOf[A](g: Gen[A]): SGen[List[A]] =
      SGen(n => g.listOfN(n))

    val gen  = Gen.unit(42)
    val prop = forAll(listOf(gen))(l => l.forall(_ == 42))
    prop.run(100, 100, RNG.Simple(System.currentTimeMillis)) shouldBe Passed
  }

  test("test FP IN SCALA LIB section Property Based Testing 7") {
    def listOf1[A](g: Gen[A]): SGen[List[A]] =
      SGen(n => g.listOfN(n max 10))

    val prop = forAll(listOf1(Gen.choose(0, 10)))(l => l.size == 1 && l.forall(_ < 10))
    prop.run(100, 100, RNG.Simple(System.currentTimeMillis))
  }

  test("test FP IN SCALA LIB section Property Based Testing 8") {
    val prop = forAll(listOf(Gen.choose(0, 20))) { l =>
      val index = Gen.choose(0, 20).sample.run(RNG.Simple(47))._1
      l.takeWhile(_ < index) ++ l.dropWhile(_ < index) == l
    }
    prop.run(100, 100, RNG.Simple(System.currentTimeMillis)) shouldBe Passed
  }

}
