package org.scala_exercises.std_lib

import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

class SequencesAndArraysTestSuite extends AnyFunSuiteLike with Matchers {
  test("test STD LIB section SequencesAndArrays 0") {
    val l = List(1, 2, 3)
    val a = l.toArray
    a should equal(Array(1, 2, 3))
  }

  test("test STD LIB section SequencesAndArrays 1") {
    val a = Array(1, 2, 3)
    val s = a.toSeq
    val l = s.toList
    l should equal(List(1, 2, 3))
  }

  test("test STD LIB section SequencesAndArrays 2") {
    val s = for (v <- 1 to 4) yield v
    s.toList should be(List(1, 2, 3, 4))
  }

  test("test STD LIB section SequencesAndArrays 3") {
    val s = for (v <- 1 to 10 if v % 3 == 0) yield v
    s.toList should be(List(3, 6, 9))
  }

  test("test STD LIB section SequencesAndArrays 4") {
    val s        = Seq("hello", "to", "you")
    val filtered = s.filter(_.length > 2)
    filtered should be(Seq("hello", "you"))
  }

  test("test STD LIB section SequencesAndArrays 5") {
    val a        = Array("hello", "to", "you", "again")
    val filtered = a.filter(_.length > 3)
    filtered should be(Array("hello", "again"))
  }

  test("test STD LIB section SequencesAndArrays 6") {
    val s = Seq("hello", "world")
    val r = s map {
      _.reverse
    }

    r should be(Seq("olleh", "dlrow"))
  }

}
