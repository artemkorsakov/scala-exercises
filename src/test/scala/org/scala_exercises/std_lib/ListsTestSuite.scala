package org.scala_exercises.std_lib

import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

class ListsTestSuite extends AnyFunSuiteLike with Matchers {
  test("test STD LIB section Lists 1") {
    val a = List(1, 2, 3)
    val b = List(1, 2, 3)
    (a eq b) should be(false)
  }

  test("test STD LIB section Lists 2") {
    val a = List(1, 2, 3)
    val b = List(1, 2, 3)
    (a == b) should be(true)
  }

  test("test STD LIB section Lists 3") {
    val a: List[String] = Nil
    val b: List[Int]    = Nil

    (a == Nil) should be(true)
    (a eq Nil) should be(true)

    (b == Nil) should be(true)
    (b eq Nil) should be(true)

    (a == b) should be(true)
    (a eq b) should be(true)
  }

  test("test STD LIB section Lists 4") {
    val a = List(1, 2, 3)
    a should equal(List(1, 2, 3))
  }

  test("test STD LIB section Lists 5") {
    val a = List(1, 2, 3)
    a.headOption should equal(Some(1))
    a.tail should equal(List(2, 3))
  }

  test("test STD LIB section Lists 6") {
    val a = List(1, 3, 5, 7, 9)
    a(0) should equal(1)
    a(2) should equal(5)
    a(4) should equal(9)

    intercept[IndexOutOfBoundsException] {
      println(a(5))
    }
  }

  test("test STD LIB section Lists 7") {
    val a = List(1, 3, 5, 7, 9)
    val b = a.filterNot(v => v == 5) // remove where value is 5

    a should equal(List(1, 3, 5, 7, 9))
    b should equal(List(1, 3, 7, 9))
  }

  test("test STD LIB section Lists 8") {

    val a = List(1, 3, 5, 7, 9)

    // get the length of the list
    a.length should equal(5)

    // reverse the list
    a.reverse should equal(List(9, 7, 5, 3, 1))

    // map a function to double the numbers over the list
    a.map { v =>
      v * 2
    } should equal(List(2, 6, 10, 14, 18))

    // filter any values divisible by 3 in the list
    a.filter { v =>
      v % 3 == 0
    } should equal(List(3, 9))
  }

  test("test STD LIB section Lists 9") {
    val a = List(1, 2, 3)

    a.map {
      _ * 2
    } should equal(List(2, 4, 6))

    a.filter {
      _ % 2 == 0
    } should equal(List(2))
  }

  test("test STD LIB section Lists 10") {
    val a = List(1, 2, 3)
    a.map(_ * 2) should equal(List(2, 4, 6))
    a.filter(_ % 2 != 0) should equal(List(1, 3))
  }

  test("test STD LIB section Lists 11") {
    val a = List(1, 3, 5, 7)
    a.reduceLeft(_ + _) should equal(16)
    a.reduceLeft(_ * _) should equal(105)
  }

  test("test STD LIB section Lists 12") {
    val a = List(1, 3, 5, 7)
    // NOTE: foldLeft uses a form called currying that we will explore later
    a.foldLeft(0)(_ + _) should equal(16)
    a.foldLeft(10)(_ + _) should equal(26)
    a.foldLeft(1)(_ * _) should equal(105)
    a.foldLeft(0)(_ * _) should equal(0)
  }

  test("test STD LIB section Lists 13") {
    val a = (1 to 5).toList
    a should be(List(1, 2, 3, 4, 5))
  }

  test("test STD LIB section Lists 14") {
    val a = List(1, 3, 5, 7)

    0 :: a should be(List(0, 1, 3, 5, 7))
  }

  test("test STD LIB section Lists 15") {
    val head = List(1, 3)
    val tail = List(5, 7)

    head ::: tail should be(List(1, 3, 5, 7))
    head ::: Nil should be(List(1, 3))
  }

  test("test STD LIB section Lists 16") {
    val d = Nil
    val c = 3 :: d
    val b = 2 :: c
    val a = 1 :: b

    a should be(List(1, 2, 3))
    a.tail should be(List(2, 3))
    b.tail should be(List(3))
    c.tail should be(Nil)
  }

}
