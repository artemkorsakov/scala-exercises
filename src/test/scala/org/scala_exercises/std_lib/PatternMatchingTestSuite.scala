package org.scala_exercises.std_lib

import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

class PatternMatchingTestSuite extends AnyFunSuiteLike with Matchers {
  test("test STD LIB section PatternMatching 0") {
    val stuff = "blue"

    val myStuff = stuff match {
      case "red" =>
        println("RED"); 1
      case "blue" =>
        println("BLUE"); 2
      case "green" =>
        println("GREEN"); 3
      case _ =>
        println(stuff); 0 // case _ will trigger if all other cases fail.
    }

    myStuff should be(2)
  }

  test("test STD LIB section PatternMatching 1") {
    val stuff = "blue"

    val myStuff = stuff match {
      case "red"   => (255, 0, 0)
      case "green" => (0, 255, 0)
      case "blue"  => (0, 0, 255)
      case _       => println(stuff); 0
    }

    myStuff should be((0, 0, 255))
  }

  test("test STD LIB section PatternMatching 2") {
    def goldilocks(expr: Any) =
      expr match {
        case ("porridge", "Papa") => "Papa eating porridge"
        case ("porridge", "Mama") => "Mama eating porridge"
        case ("porridge", "Baby") => "Baby eating porridge"
        case _                    => "what?"
      }

    goldilocks(("porridge", "Mama")) should be("Mama eating porridge")
  }

  test("test STD LIB section PatternMatching 3") {
    def goldilocks(expr: Any) =
      expr match {
        case ("porridge", _)   => "eating"
        case ("chair", "Mama") => "sitting"
        case ("bed", "Baby")   => "sleeping"
        case _                 => "what?"
      }

    goldilocks(("porridge", "Papa")) should be("eating")
    goldilocks(("chair", "Mama")) should be("sitting")
  }

  test("test STD LIB section PatternMatching 4") {
    def goldilocks(expr: (String, String)) =
      expr match {
        case ("porridge", bear) =>
          bear + " said someone's been eating my porridge"
        case ("chair", bear) => bear + " said someone's been sitting in my chair"
        case ("bed", bear)   => bear + " said someone's been sleeping in my bed"
        case _               => "what?"
      }

    goldilocks(("porridge", "Papa")) should be("Papa said someone's been eating my porridge")
    goldilocks(("chair", "Mama")) should be("Mama said someone's been sitting in my chair")
  }

  test("test STD LIB section PatternMatching 5") {
    val foodItem = "porridge"

    def goldilocks(expr: (String, String)) =
      expr match {
        case (`foodItem`, _)   => "eating"
        case ("chair", "Mama") => "sitting"
        case ("bed", "Baby")   => "sleeping"
        case _                 => "what?"
      }

    goldilocks(("porridge", "Papa")) should be("eating")
    goldilocks(("chair", "Mama")) should be("sitting")
    goldilocks(("porridge", "Cousin")) should be("eating")
    goldilocks(("beer", "Cousin")) should be("what?")
  }

  test("test STD LIB section PatternMatching 6") {
    def patternEquals(i: Int, j: Int) =
      j match {
        case `i` => true
        case _   => false
      }
    patternEquals(3, 3) should be(true)
    patternEquals(7, 9) should be(false)
    patternEquals(9, 9) should be(true)
  }

  test("test STD LIB section PatternMatching 7") {
    val secondElement = List(1, 2, 3) match {
      case x :: xs => xs.head
      case _       => 0
    }

    secondElement should be(2)
  }

  test("test STD LIB section PatternMatching 8") {
    val secondElement = List(1, 2, 3) match {
      case x :: y :: xs => y
      case _            => 0
    }

    secondElement should be(2)
  }

  test("test STD LIB section PatternMatching 9") {
    val secondElement = List(1) match {
      case x :: y :: xs => y // only matches a list with two or more items
      case _            => 0
    }

    secondElement should be(0)
  }

  test("test STD LIB section PatternMatching 10") {
    val r = List(1, 2, 3) match {
      case x :: y :: Nil => y // only matches a list with exactly two items
      case _             => 0
    }

    r should be(0)
  }

  test("test STD LIB section PatternMatching 11") {
    val r = List(1, 2, 3) match {
      case x :: y :: z :: tail => tail
      case _                   => 0
    }

    r == Nil should be(true)
  }

}
