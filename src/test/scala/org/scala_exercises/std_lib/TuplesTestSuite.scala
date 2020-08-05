package org.scala_exercises.std_lib

import java.util.Date

import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

class TuplesTestSuite extends AnyFunSuiteLike with Matchers {
  test("test STD LIB section Tuples") {
    val tuple  = ("apple", "dog")
    val fruit  = tuple._1
    val animal = tuple._2

    fruit should be("apple")
    animal should be("dog")

    val tuple5 = ("a", 1, 2.2, new Date(), "five")

    tuple5._2 should be(1)
    tuple5._5 should be("five")

    val student          = ("Sean Rogers", 21, 3.5)
    val (name, age, gpa) = student

    name should be("Sean Rogers")
    age should be(21)
    gpa should be(3.5)

    val tuple1 = ("apple", 3).swap
    tuple1._1 should be(3)
    tuple1._2 should be("apple")
  }
}
