package org.scala_exercises.std_lib

import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

import scala.collection.immutable.LazyList.cons

class TraversablesTestSuite extends AnyFunSuiteLike with Matchers {
  test("test STD LIB section Traversables 0") {
    val set    = Set(1, 9, 10, 22)
    val list   = List(3, 4, 5, 10)
    val result = set ++ list
    result.size should be(7)

    val result2 = list ++ set
    result2.size should be(8)
  }

  test("test STD LIB section Traversables 1") {
    val set    = Set(1, 3, 4, 6)
    val result = set.map(_ * 4)
    result.lastOption should be(Some(24))
  }

  test("test STD LIB section Traversables 2") {
    val list = List(List(1), List(2, 3, 4), List(5, 6, 7), List(8, 9, 10))
    list.flatten should be(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
  }

  test("test STD LIB section Traversables 3") {
    val list   = List(List(1), List(2, 3, 4), List(5, 6, 7), List(8, 9, 10))
    val result = list.flatMap(_.map(_ * 4))
    result should be(List(4, 8, 12, 16, 20, 24, 28, 32, 36, 40))
  }

  test("test STD LIB section Traversables 4") {
    val list   = List(1, 2, 3, 4, 5)
    val result = list.flatMap(it => if (it % 2 == 0) Some(it) else None)
    result should be(List(2, 4))
  }

  test("test STD LIB section Traversables 5") {
    val list = List(4, 6, 7, 8, 9, 13, 14)
    val result = list.collect {
      case x: Int if (x % 2 == 0) => x * 3
    }
    result should be(List(12, 18, 24, 42))
  }

  test("test STD LIB section Traversables 6") {
    val list = List(4, 6, 7, 8, 9, 13, 14)
    val partialFunction1: PartialFunction[Int, Int] = {
      case x: Int if x % 2 == 0 => x * 3
    }
    val partialFunction2: PartialFunction[Int, Int] = {
      case y: Int if y % 2 != 0 => y * 4
    }
    val result = list.collect(partialFunction1 orElse partialFunction2)
    result should be(List(12, 18, 28, 24, 36, 52, 42))
  }

  test("test STD LIB section Traversables 7") {
    val list = List(4, 6, 7, 8, 9, 13, 14)
    list.foreach(num => println(num * 4))
    list should be(List(4, 6, 7, 8, 9, 13, 14))
  }

  test("test STD LIB section Traversables 8") {
    val set    = Set(4, 6, 7, 8, 9, 13, 14)
    val result = set.toArray
    result.isInstanceOf[Array[Int]] should be(true)
  }

  test("test STD LIB section Traversables 9") {
    val set    = Set(4, 6, 7, 8, 9, 13, 14)
    val result = set.toList

    result.isInstanceOf[List[_]] should be(true)
  }

  test("test STD LIB section Traversables 10") {
    val list   = List(5, 6, 7, 8, 9)
    val result = list.toList
    result eq list should be(true)
  }

  test("test STD LIB section Traversables 11") {
    val set    = Set(4, 6, 7, 8, 9, 13, 14)
    val result = set.toIterable
    result.isInstanceOf[Iterable[_]] should be(true)
  }

  test("test STD LIB section Traversables 12") {
    val set    = Set(4, 6, 7, 8, 9, 13, 14)
    val result = set.toSeq
    result.isInstanceOf[Seq[_]] should be(true)
  }

  test("test STD LIB section Traversables 13") {
    val set    = Set(4, 6, 7, 8, 9, 13, 14)
    val result = set.toIndexedSeq
    result.isInstanceOf[IndexedSeq[_]] should be(true)
  }

  test("test STD LIB section Traversables 14") {
    val list   = List(4, 6, 7, 8, 9, 13, 14)
    val result = list.to(LazyList)
    result.isInstanceOf[LazyList[_]] should be(true)
    (result take 3) should be(LazyList(4, 6, 7))
  }

  test("test STD LIB section Traversables 15") {
    val list   = List(4, 6, 7, 8, 9, 13, 14)
    val result = list.toSet
    result.isInstanceOf[Set[_]] should be(true)
  }

  test("test STD LIB section Traversables 16") {
    val list   = List("Phoenix" -> "Arizona", "Austin" -> "Texas")
    val result = list.toMap
    result.isInstanceOf[Map[_, _]] should be(true)
  }

  test("test STD LIB section Traversables 17") {
    val set    = Set("Phoenix" -> "Arizona", "Austin" -> "Texas")
    val result = set.toMap
    result.isInstanceOf[Map[_, _]] should be(true)
  }

  test("test STD LIB section Traversables 18") {
    val map = Map("Phoenix" -> "Arizona", "Austin" -> "Texas")
    map.isEmpty should be(false)

    val set = Set()
    set.isEmpty should be(true)
  }

  test("test STD LIB section Traversables 19") {
    val map = Map("Phoenix" -> "Arizona", "Austin" -> "Texas")
    map.nonEmpty should be(true)

    val set = Set()
    set.nonEmpty should be(false)
  }

  test("test STD LIB section Traversables 20") {
    val map = Map("Phoenix" -> "Arizona", "Austin" -> "Texas")
    map.size should be(2)
  }

  test("test STD LIB section Traversables 21") {
    val map = Map("Phoenix" -> "Arizona", "Austin" -> "Texas")
    map.knownSize should be(2)

    val stream = cons(0, cons(1, LazyList.empty))
    stream.knownSize should be(-1)
  }

  test("test STD LIB section Traversables 22") {
    val list = List(10, 19, 45, 1, 22)
    list.head should be(10)
  }

  test("test STD LIB section Traversables 23") {
    val list = List(10, 19, 45, 1, 22)
    list.headOption should be(Some(10))

    val list2 = List()
    list2.headOption should be(None)
  }

  test("test STD LIB section Traversables 24") {
    val list = List(10, 19, 45, 1, 22)
    list.last should be(22)
  }

  test("test STD LIB section Traversables 25") {
    val list = List(10, 19, 45, 1, 22)
    list.lastOption should be(Some(22))

    val list2 = List()
    list2.lastOption should be(None)
  }

  test("test STD LIB section Traversables 26") {
    val list = List(10, 19, 45, 1, 22)
    list.find(_ % 2 != 0) should be(Some(19))

    val list2 = List(4, 8, 16)
    list2.find(_ % 2 != 0) should be(None)
  }

  test("test STD LIB section Traversables 27") {
    val list = List(10, 19, 45, 1, 22)
    list.tail should be(List(19, 45, 1, 22))
  }

  test("test STD LIB section Traversables 28") {
    val list = List(10, 19, 45, 1, 22)
    list.init should be(List(10, 19, 45, 1))
  }

  test("test STD LIB section Traversables 29") {
    val list = List(10, 19, 45, 1, 22)
    list.slice(1, 3) should be(List(19, 45))
  }

  test("test STD LIB section Traversables 30") {
    val list = List(10, 19, 45, 1, 22)
    list.take(3) should be(List(10, 19, 45))
  }

  test("test STD LIB section Traversables 31") {
    def makeLazyList(v: Int): LazyList[Int] = cons(v, makeLazyList(v + 1))
    val a                                   = makeLazyList(2)
    (a take 3 toList) should be(List(2, 3, 4))
  }

  test("test STD LIB section Traversables 32") {
    def makeLazyList(v: Int): LazyList[Int] = cons(v, makeLazyList(v + 1))
    val a                                   = makeLazyList(2)
    ((a drop 6) take 3).toList should be(List(8, 9, 10))
  }

  test("test STD LIB section Traversables 33") {
    val list = List(87, 44, 5, 4, 200, 10, 39, 100)
    list.takeWhile(_ < 100) should be(List(87, 44, 5, 4))
  }

  test("test STD LIB section Traversables 34") {
    val list = List(87, 44, 5, 4, 200, 10, 39, 100)
    list.dropWhile(_ < 100) should be(List(200, 10, 39, 100))
  }

  test("test STD LIB section Traversables 35") {
    val array = Array(87, 44, 5, 4, 200, 10, 39, 100)
    array.filter(_ < 100) should be(Array(87, 44, 5, 4, 10, 39))
  }

  test("test STD LIB section Traversables 36") {
    val array = Array(87, 44, 5, 4, 200, 10, 39, 100)
    array.filterNot(_ < 100) should be(Array(200, 100))
  }

  test("test STD LIB section Traversables 37") {
    val array  = Array(87, 44, 5, 4, 200, 10, 39, 100)
    val result = array splitAt 3
    result._1 should be(Array(87, 44, 5))
    result._2 should be(Array(4, 200, 10, 39, 100))
  }

  test("test STD LIB section Traversables 38") {
    val array  = Array(87, 44, 5, 4, 200, 10, 39, 100)
    val result = array span (_ < 100)
    result._1 should be(Array(87, 44, 5, 4))
    result._2 should be(Array(200, 10, 39, 100))
  }

  test("test STD LIB section Traversables 39") {
    val array  = Array(87, 44, 5, 4, 200, 10, 39, 100)
    val result = array partition (_ < 100)
    result._1 should be(Array(87, 44, 5, 4, 10, 39))
    result._2 should be(Array(200, 100))
  }

  test("test STD LIB section Traversables 40") {
    val array = Array(87, 44, 5, 4, 200, 10, 39, 100)

    val oddAndSmallPartial: PartialFunction[Int, String] = {
      case x: Int if x % 2 != 0 && x < 100 => "Odd and less than 100"
    }

    val evenAndSmallPartial: PartialFunction[Int, String] = {
      case x: Int if x != 0 && x % 2 == 0 && x < 100 => "Even and less than 100"
    }

    val negativePartial: PartialFunction[Int, String] = {
      case x: Int if x < 0 => "Negative Number"
    }

    val largePartial: PartialFunction[Int, String] = {
      case x: Int if x > 99 => "Large Number"
    }

    val zeroPartial: PartialFunction[Int, String] = {
      case x: Int if x == 0 => "Zero"
    }

    val result = array groupBy {
      oddAndSmallPartial orElse
      evenAndSmallPartial orElse
      negativePartial orElse
      largePartial orElse
      zeroPartial
    }

    (result("Even and less than 100") size) should be(3)
    (result("Large Number") size) should be(2)
  }

  test("test STD LIB section Traversables 41") {
    val list   = List(87, 44, 5, 4, 200, 10, 39, 100)
    val result = list forall (_ < 100)
    result should be(false)
  }

  test("test STD LIB section Traversables 42") {
    val list   = List(87, 44, 5, 4, 200, 10, 39, 100)
    val result = list exists (_ < 100)
    result should be(true)
  }

  test("test STD LIB section Traversables 43") {
    val list   = List(87, 44, 5, 4, 200, 10, 39, 100)
    val result = list count (_ < 100)
    result should be(6)
  }

  test("test STD LIB section Traversables 44") {
    val list   = List(5, 4, 3, 2, 1)
    val result = list.foldLeft(0)((`running total`, `next element`) => `running total` - `next element`)
    result should be(-15)

    val result2 = list.foldLeft(0)(_ - _) //Short hand
    result2 should be(-15)

    (((((0 - 5) - 4) - 3) - 2) - 1) should be(-15)
  }

  test("test STD LIB section Traversables 45") {
    val list   = List(5, 4, 3, 2, 1)
    val result = list.foldRight(0)((`next element`, `running total`) => `next element` - `running total`)
    result should be(3)

    val result2 = list.foldRight(0)(_ - _) //Short hand
    result2 should be(3)

    (5 - (4 - (3 - (2 - (1 - 0))))) should be(3)
  }

  test("test STD LIB section Traversables 46") {
    val intList = List(5, 4, 3, 2, 1)
    intList.reduceLeft(_ + _) should be(15)

    val stringList = List("Do", "Re", "Me", "Fa", "So", "La", "Te", "Do")
    stringList.reduceLeft(_ + _) should be("DoReMeFaSoLaTeDo")
  }

  test("test STD LIB section Traversables 47") {
    val intList = List(5, 4, 3, 2, 1)
    intList.reduceRight(_ + _) should be(15)

    val stringList = List("Do", "Re", "Me", "Fa", "So", "La", "Te", "Do")
    stringList.reduceRight(_ + _) should be("DoReMeFaSoLaTeDo")
  }

  test("test STD LIB section Traversables 48") {
    val intList = List(5, 4, 3, 2, 1)
    intList.sum should be(15)
    intList.product should be(120)
    intList.max should be(5)
    intList.min should be(1)
  }

  test("test STD LIB section Traversables 49") {
    val intList = List(5, 4, 3, 2, 1)
    intList.reduceRight((x, y) => x - y) should be(3)
    intList.reverse.reduceLeft((x, y) => y - x) should be(3)
    intList.reverse.reduce((x, y) => y - x) should be(3)
  }

  test("test STD LIB section Traversables 50") {
    val list = List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))
    list.transpose should be(List(List(1, 4, 7), List(2, 5, 8), List(3, 6, 9)))

    val list2 = List(List(1), List(4))
    list2.transpose should be(List(List(1, 4)))
  }

  test("test STD LIB section Traversables 51") {
    val list = List(1, 2, 3, 4, 5)
    list.mkString(",") should be("1,2,3,4,5")
  }

  test("test STD LIB section Traversables 52") {
    val list = List(1, 2, 3, 4, 5)
    list.mkString(">", ",", "<") should be(">1,2,3,4,5<")
  }

  test("test STD LIB section Traversables 53") {
    val stringBuilder = new StringBuilder()
    val list          = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)
    stringBuilder.append("I want all numbers 6-12: ")
    list.filter(it => it > 5 && it < 13).addString(stringBuilder, ",")
    stringBuilder.mkString should be("I want all numbers 6-12: 6,7,8,9,10,11,12")
  }

}
