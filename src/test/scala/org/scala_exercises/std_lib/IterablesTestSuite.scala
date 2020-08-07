package org.scala_exercises.std_lib

import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

class IterablesTestSuite extends AnyFunSuiteLike with Matchers {
  test("test STD LIB section Iterables 0") {
    val list = List(3, 5, 9, 11, 15, 19, 21)
    val it   = list.iterator
    if (it.hasNext)
      it.next should be(3)
  }

  test("test STD LIB section Iterables 1") {
    val list = List(3, 5, 9, 11, 15, 19, 21, 24, 32)
    val it   = list grouped 3
    it.next() should be(List(3, 5, 9))
    it.next() should be(List(11, 15, 19))
    it.next() should be(List(21, 24, 32))
  }

  test("test STD LIB section Iterables 2") {
    val list = List(3, 5, 9, 11, 15, 19, 21, 24, 32)
    val it   = list sliding 3
    it.next() should be(List(3, 5, 9))
    it.next() should be(List(5, 9, 11))
    it.next() should be(List(9, 11, 15))
  }

  test("test STD LIB section Iterables 3") {
    val list = List(3, 5, 9, 11, 15, 19, 21, 24, 32)
    val it   = list sliding (3, 3)
    it.next() should be(List(3, 5, 9))
    it.next() should be(List(11, 15, 19))
    it.next() should be(List(21, 24, 32))
  }

  test("test STD LIB section Iterables 4") {
    val list = List(3, 5, 9, 11, 15, 19, 21, 24, 32)
    (list takeRight 3) should be(List(21, 24, 32))
  }

  test("test STD LIB section Iterables 5") {
    val list = List(3, 5, 9, 11, 15, 19, 21, 24, 32)
    (list dropRight 3) should be(List(3, 5, 9, 11, 15, 19))
  }

  test("test STD LIB section Iterables 6") {
    val xs = List(3, 5, 9)
    val ys = List("Bob", "Ann", "Stella")
    (xs zip ys) should be(List((3, "Bob"), (5, "Ann"), (9, "Stella")))
  }

  test("test STD LIB section Iterables 7") {
    val xs = List(3, 5, 9)
    val ys = List("Bob", "Ann")
    (xs zip ys) should be(List((3, "Bob"), (5, "Ann")))
  }

  test("test STD LIB section Iterables 8") {
    val xs = List(3, 5, 9)
    val ys = List("Bob", "Ann")
    (xs zipAll (ys, -1, "?")) should be(List((3, "Bob"), (5, "Ann"), (9, "?")))

    val xt = List(3, 5)
    val yt = List("Bob", "Ann", "Stella")
    (xt zipAll (yt, -1, "?")) should be(List((3, "Bob"), (5, "Ann"), (-1, "Stella")))
  }

  test("test STD LIB section Iterables 9") {
    val xs = List("Manny", "Moe", "Jack")
    xs.zipWithIndex should be(List(("Manny", 0), ("Moe", 1), ("Jack", 2)))
  }

  test("test STD LIB section Iterables 10") {
    val xs = List("Manny", "Moe", "Jack")
    val ys = List("Manny", "Moe", "Jack")
    xs.iterator.sameElements(ys) should be(true)

    val xt = List("Manny", "Moe", "Jack")
    val yt = List("Manny", "Jack", "Moe")
    xt.iterator.sameElements(yt) should be(false)

    val xs1 = Set(3, 2, 1, 4, 5, 6, 7)
    val ys1 = Set(7, 2, 1, 4, 5, 6, 3)
    xs1.iterator.sameElements(ys1) should be(true)

    val xt1 = Set(1, 2, 3)
    val yt1 = Set(3, 2, 1)
    xt1.iterator.sameElements(yt1) should be(false)
  }

}
