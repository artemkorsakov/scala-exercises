package org.scala_exercises.cats

import cats.Eval
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

class EvalTestSuite extends AnyFunSuiteLike with Matchers {
  test("test CATS LIB section Eval 0") {
    val eager = Eval.now {
      println("Running expensive calculation...")
      1 + 2 * 3
    }

    eager.value shouldBe 7
  }

  test("test CATS LIB section Eval 1") {
    val eagerEval = Eval.now {
      println("This is eagerly evaluated")
      1 :: 2 :: 3 :: Nil
    }

    eagerEval.value shouldBe List(1, 2, 3)
  }

  test("test CATS LIB section Eval 2") {
    val lazyEval = Eval.later {
      println("Running expensive calculation...")
      1 + 2 * 3
    }
    // lazyEval: cats.Eval[Int] = cats.Later@6c2b03e9

    lazyEval.value shouldBe 7
    // Running expensive calculation...

    lazyEval.value shouldBe 7
  }

  test("test CATS LIB section Eval 3") {
    val n       = 2
    var counter = 0
    val lazyEval = Eval.later {
      println("This is lazyly evaluated with caching")
      counter = counter + 1
      1 to n
    }

    //when/then
    List.fill(n)("").foreach(_ => lazyEval.value)
    lazyEval.value shouldBe List(1, 2)
    counter shouldBe 1
  }

  test("test CATS LIB section Eval 4") {
    val n       = 4
    var counter = 0
    val alwaysEval = Eval.always {
      println("This is lazyly evaluated without caching")
      counter = counter + 1
      1 to n
    }

    //when/then
    List.fill(n)("").foreach(_ => alwaysEval.value)
    counter shouldBe 4
    alwaysEval.value shouldBe List(1, 2, 3, 4)
    counter shouldBe 5
  }

  test("test CATS LIB section Eval 5") {
    val list = List.fill(3)(0)

    //when
    val deferedEval: Eval[List[Int]] = Eval.now(list).flatMap(e => Eval.defer(Eval.later(e)))

    //then
    Eval.defer(deferedEval).value shouldBe List(0, 0, 0)
  }

}
