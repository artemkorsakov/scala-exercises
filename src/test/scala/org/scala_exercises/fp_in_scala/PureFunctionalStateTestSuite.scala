package org.scala_exercises.fp_in_scala

import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers
import fpinscalalib.customlib.state.RNG.Simple
import fpinscalalib.customlib.state.RNG._
import fpinscalalib.customlib.state._

class PureFunctionalStateTestSuite extends AnyFunSuiteLike with Matchers {
  test("test FP IN SCALA LIB section Pure Functional State 0") {
    def nonNegativeInt(rng: RNG): (Int, RNG) = {
      val (i, r) = rng.nextInt
      (if (i < 0) -(i + 1) else i, r)
    }

    val rng             = Simple(47)
    val (result1, rng1) = nonNegativeInt(rng)
    val result2         = nonNegativeInt(rng1)._1

    result1 should be >= 0
    result2 should be >= 0
    result1 should not be result2
  }

  test("test FP IN SCALA LIB section Pure Functional State 1") {
    def double(rng: RNG): (Double, RNG) = {
      val (i, r) = nonNegativeInt(rng)
      (i / (Int.MaxValue.toDouble + 1), r)
    }

    val rng             = Simple(47)
    val (double1, rng1) = double(rng)
    val double2         = double(rng1)._1

    double1.toInt should be >= 0
    double2.toInt should be >= 0
    double1 should not be double2
  }

  test("test FP IN SCALA LIB section Pure Functional State 2") {
    def ints(count: Int)(rng: RNG): (List[Int], RNG) =
      if (count == 0)
        (List(), rng)
      else {
        val (x, r1)  = rng.nextInt
        val (xs, r2) = ints(count - 1)(r1)
        (x :: xs, r2)
      }

    val (list1, rng1) = ints(5)(Simple(47))
    val list2         = ints(5)(rng1)._1
    list1.size shouldBe 5
    list1.headOption should not be list2
  }

  test("test FP IN SCALA LIB section Pure Functional State 3") {
    val double: Rand[Double] =
      map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

    val rng             = Simple(47)
    val (double1, rng2) = double(rng)
    val double2         = double(rng2)._1

    double1.toInt should be >= 0
    double2.toInt should be >= 0
    double1 should not be double2
  }

  test("test FP IN SCALA LIB section Pure Functional State 4") {
    def nonNegativeLessThan(n: Int): Rand[Int] =
      flatMap(nonNegativeInt) { i =>
        val mod = i % n
        if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
      }

    val (result1, rng1) = nonNegativeLessThan(10)(Simple(47))
    val result2         = nonNegativeLessThan(10)(rng1)._1

    result1 should be >= 0
    result1 should be < 10
    result2 should be >= 0
    result2 should be < 10
    result1 should not be result2
  }

  test("test FP IN SCALA LIB section Pure Functional State 5") {
    def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)

    val (dice1, rng1) = rollDie(Simple(47))
    val dice2         = rollDie(rng1)._1
    dice1 should be > 0
    dice1 should be < 6
    dice2 should be > 0
    dice2 should be < 6
    dice1 should not be dice2
  }

  test("test FP IN SCALA LIB section Pure Functional State 6") {
    import fpinscalalib.customlib.state.Machine
    import fpinscalalib.customlib.state.State
    import fpinscalalib.customlib.state.State._

    object Candy {
      def update =
        (i: Input) =>
          (s: Machine) =>
            (i, s) match {
              case (_, Machine(_, 0, _))        => s
              case (Coin, Machine(false, _, _)) => s
              case (Turn, Machine(true, _, _))  => s
              case (Coin, Machine(true, candy, coin)) =>
                Machine(false, candy, coin + 1)
              case (Turn, Machine(false, candy, coin)) =>
                Machine(true, candy - 1, coin)
            }

      def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
        for {
          _ <- sequence(inputs map (modify[Machine] _ compose update))
          s <- get
        } yield (s.coins, s.candies)
    }

    import Candy._

    val inputCoin = List(Coin)
    val inputTurn = List(Turn)

    // Inserting a coin into a locked machine will cause it to unlock if there’s any candy left.
    val machine1 = Machine(true, 1, 0)
    simulateMachine(inputCoin).run(machine1)._2.locked shouldBe false

    // Turning the knob on an unlocked machine will cause it to dispense candy and become locked.
    val machine2 = Machine(false, 1, 1)
    val m2Result = simulateMachine(inputTurn).run(machine2)
    m2Result._2.locked shouldBe true
    m2Result._2.candies shouldBe 0

    // Turning the knob on a locked machine or inserting a coin into an unlocked machine does nothing.
    simulateMachine(inputTurn).run(machine1)._2.locked shouldBe machine1.locked
    simulateMachine(inputCoin).run(machine2)._2.locked shouldBe machine2.locked

    // A machine that’s out of candy ignores all inputs.
    val machine3 = Machine(true, 0, 1)
    simulateMachine(inputTurn).run(machine3)._2.locked shouldBe machine3.locked
    simulateMachine(inputCoin).run(machine3)._2.locked shouldBe machine3.locked
  }

}
