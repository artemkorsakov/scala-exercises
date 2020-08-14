package org.scala_exercises.fp_in_scala

import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers
import fpinscalalib.customlib.functionalparallelism.Par
import fpinscalalib.customlib.functionalparallelism.Par._
import java.util.concurrent.Executors

class PurelyFunctionalParallelismTestSuite extends AnyFunSuiteLike with Matchers {
  test("test FP IN SCALA LIB section Purely Functional Parallelism 0") {
    def asyncF[A, B](f: A => B): A => Par[B] =
      a => lazyUnit(f(a))

    def asyncIntToString = asyncF((x: Int) => x.toString)
    val executorService  = Executors.newFixedThreadPool(2)

    Par.run(executorService)(asyncIntToString(10)).get() shouldBe "10"
  }

  test("test FP IN SCALA LIB section Purely Functional Parallelism 1") {
    def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = {
      val pars: List[Par[List[A]]] =
        l map asyncF((a: A) => if (f(a)) List(a) else List())
      map(sequence(pars))(_.flatten)
    }

    val filterOp        = parFilter(List(1, 2, 3, 4, 5))(_ < 4)
    val executorService = Executors.newCachedThreadPool()
    val result          = Par.run(executorService)(filterOp).get()
    result shouldBe List(1, 2, 3)
  }

  test("test FP IN SCALA LIB section Purely Functional Parallelism 2") {
    def choiceViaChoiceN[A](a: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] =
      choiceN(map(a)(b => if (b) 0 else 1))(List(ifTrue, ifFalse))

    val executorService = Executors.newFixedThreadPool(2)
    val choice          = choiceViaChoiceN(Par.unit(true))(Par.unit(1), Par.unit(2))
    choice.apply(executorService).get() shouldBe 1
  }

  test("test FP IN SCALA LIB section Purely Functional Parallelism 3") {
    def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
      es => {
        val k = Par.run(es)(key).get
        Par.run(es)(choices(k))
      }

    val executorService = Executors.newFixedThreadPool(2)
    val choicesMap      = Map("a" -> Par.unit(1), "b" -> Par.unit(2))

    choiceMap(Par.unit("b"))(choicesMap).apply(executorService).get() shouldBe 2
  }

  test("test FP IN SCALA LIB section Purely Functional Parallelism 4") {
    def chooser[A, B](p: Par[A])(choices: A => Par[B]): Par[B] =
      es => {
        val k = Par.run(es)(p).get
        Par.run(es)(choices(k))
      }

    val choices = (a: Int) => {
      if (a % 2 == 0) Par.unit("even")
      else Par.unit("odd")
    }

    val executorService = Executors.newFixedThreadPool(2)
    chooser(Par.unit(1))(choices).apply(executorService).get() shouldBe "odd"
  }

  test("test FP IN SCALA LIB section Purely Functional Parallelism 5") {
    def choiceViaFlatMap[A](p: Par[Boolean])(f: Par[A], t: Par[A]): Par[A] =
      flatMap(p)(b => if (b) t else f)

    val executorService = Executors.newFixedThreadPool(2)
    val choice          = choiceViaFlatMap(Par.unit(false))(Par.unit("a"), Par.unit("b"))
    choice.apply(executorService).get() shouldBe "a"
  }

  test("test FP IN SCALA LIB section Purely Functional Parallelism 6") {
    def choiceNViaFlatMap[A](p: Par[Int])(choices: List[Par[A]]): Par[A] =
      flatMap(p)(i => choices(i))

    val executorService = Executors.newFixedThreadPool(2)
    val choices         = List(Par.unit("a"), Par.unit("b"), Par.unit("c"))
    choiceNViaFlatMap(Par.unit(2))(choices).apply(executorService).get() shouldBe "c"
  }

  test("test FP IN SCALA LIB section Purely Functional Parallelism 7") {
    val nestedPar       = Par.unit(Par.unit("foo"))
    val executorService = Executors.newFixedThreadPool(2)

    joinViaFlatMap(nestedPar)(executorService).get() shouldBe "foo"
  }

}
