package org.scala_exercises.cats

import cats._
import cats.implicits._
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

class MonoidTestSuite extends AnyFunSuiteLike with Matchers {
  test("test STD LIB section Monoid 0") {
    Monoid[String].empty should be("")
    Monoid[String].combineAll(List("a", "b", "c")) should be("abc")
    Monoid[String].combineAll(List()) should be("")
  }

  test("test STD LIB section Monoid 1") {
    Monoid[Map[String, Int]].combineAll(List(Map("a" -> 1, "b" -> 2), Map("a" -> 3))) should be(Map("a" -> 4, "b" -> 2))
    Monoid[Map[String, Int]].combineAll(List()) should be(Map.empty[String, Int])
  }

  test("test STD LIB section Monoid 2") {
    val l = List(1, 2, 3, 4, 5)
    l.foldMap(identity) should be(15)
    l.foldMap(i => i.toString) should be("12345")
  }

  test("test STD LIB section Monoid 3") {
    val l = List(1, 2, 3, 4, 5)
    l.foldMap(i => (i, i.toString)) should be((15, "12345"))
  }

}
