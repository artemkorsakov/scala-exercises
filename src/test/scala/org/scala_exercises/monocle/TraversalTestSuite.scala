package org.scala_exercises.monocle

import monocle.Traversal
import org.scala_exercises.monocle.TraversalHelper._
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

object TraversalHelper {

  val xs = List(1, 2, 3, 4, 5)

  import cats.implicits._ // to get all cats instances including Traverse[List]
  val eachL = Traversal.fromTraverse[List, Int]

  case class Point(id: String, x: Int, y: Int)

  val points = Traversal.apply2[Point, Int](_.x, _.y)((x, y, p) => p.copy(x = x, y = y))

  import _root_.alleycats.std.map._
  import cats.Applicative

  def filterKey[K, V](predicate: K => Boolean): Traversal[Map[K, V], V] =
    new Traversal[Map[K, V], V] {
      def modifyF[F[_]: Applicative](f: V => F[V])(s: Map[K, V]): F[Map[K, V]] =
        s.map {
          case (k, v) =>
            k -> (if (predicate(k)) f(v) else v.pure[F])
        }.sequence
    }

  val m = Map(1 -> "one", 2 -> "two", 3 -> "three", 4 -> "Four")

}

class TraversalTestSuite extends AnyFunSuiteLike with Matchers {
  test("test MONOCLE LIB section Traversal 0") {
    eachL.set(0)(xs) should be(List(0, 0, 0, 0, 0))
    eachL.modify(_ + 1)(xs) should be(List(2, 3, 4, 5, 6))
  }

  test("test MONOCLE LIB section Traversal 1") {
    eachL.getAll(xs) should be(List(1, 2, 3, 4, 5))
    eachL.headOption(xs) should be(Some(1))
    eachL.find(_ > 3)(xs) should be(Some(4))
    eachL.all(_ % 2 == 0)(xs) should be(false)
  }

  test("test MONOCLE LIB section Traversal 2") {
    points.set(5)(Point("bottom-left", 0, 0)) should be(Point("bottom-left", 5, 5))
  }

  test("test MONOCLE LIB section Traversal 3") {
    val filterEven = filterKey[Int, String](_ % 2 == 0)
    // filterEven: monocle.Traversal[Map[Int,String],String] = $anon$1@5fadf001

    filterEven.modify(_.toUpperCase)(m) should be(Map(4 -> "FOUR", 3 -> "three", 2 -> "TWO", 1 -> "one"))
  }

  test("test MONOCLE LIB section Traversal 4") {
    def modifyGetAll[S, A](t: Traversal[S, A], s: S, f: A => A): Boolean =
      t.getAll(t.modify(f)(s)) == t.getAll(s).map(f)

    def composeModify[S, A](t: Traversal[S, A], s: S, f: A => A, g: A => A): Boolean =
      t.modify(g)(t.modify(f)(s)) == t.modify(g compose f)(s)

    modifyGetAll(eachL, List(1, 2, 3), (x: Int) => x + 1) should be(true)

    composeModify(eachL, List(1, 2, 3), (x: Int) => x + 1, (y: Int) => y + 2) should be(true)
  }

}
