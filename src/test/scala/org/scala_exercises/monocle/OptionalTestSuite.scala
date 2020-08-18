package org.scala_exercises.monocle

import monocle.Optional
import org.scala_exercises.monocle.OptionalHelper._
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

object OptionalHelper {

  val head = Optional[List[Int], Int] {
    case Nil     => None
    case x :: xs => Some(x)
  } { a =>
    {
      case Nil     => Nil
      case x :: xs => a :: xs
    }
  }

  val xs = List(1, 2, 3)
  val ys = List.empty[Int]

}

class OptionalTestSuite extends AnyFunSuiteLike with Matchers {
  test("test MONOCLE LIB section Optional 0") {
    head.nonEmpty(xs) should be(true)
    head.nonEmpty(ys) should be(false)
  }

  test("test MONOCLE LIB section Optional 1") {
    head.getOption(xs) should be(Some(1))
    head.set(5)(xs) should be(List(5, 2, 3))
    head.getOption(ys) should be(None)
    head.set(5)(ys) should be(List())
  }

  test("test MONOCLE LIB section Optional 2") {
    head.modify(_ + 1)(xs) should be(List(2, 2, 3))
    head.modify(_ + 1)(ys) should be(List())
  }

  test("test MONOCLE LIB section Optional 3") {
    head.modifyOption(_ + 1)(xs) should be(Some(List(2, 2, 3)))
    head.modifyOption(_ + 1)(ys) should be(None)
  }

  test("test MONOCLE LIB section Optional 4") {
    val head = Optional[List[Int], Int] {
      case Nil     => None
      case x :: xs => Some(x)
    } { a =>
      {
        case Nil     => Nil
        case x :: xs => a :: xs
      }
    }
    class OptionalLaws[S, A](optional: Optional[S, A]) {

      def getOptionSet(s: S): Boolean =
        optional.getOrModify(s).fold(identity, optional.set(_)(s)) == s

      def setGetOption(s: S, a: A): Boolean =
        optional.getOption(optional.set(a)(s)) == optional.getOption(s).map(_ => a)

    }
    new OptionalLaws(head).getOptionSet(List(1, 2, 3)) should be(true)

    new OptionalLaws(head).setGetOption(List(1, 2, 3), 20) should be(true)
  }

}
