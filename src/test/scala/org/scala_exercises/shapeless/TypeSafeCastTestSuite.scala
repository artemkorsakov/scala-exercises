package org.scala_exercises.shapeless

import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers
import shapeless._
import syntax.typeable._

class TypeSafeCastTestSuite extends AnyFunSuiteLike with Matchers {
  test("test SHAPELESS LIB section Type Safe Cast 0") {
    val l: Any = List(Vector("foo", "bar", "baz"), Vector("wibble"))
    l.cast[List[Vector[String]]] should be(Some(List(Vector("foo", "bar", "baz"), Vector("wibble"))))
    l.cast[List[Vector[Int]]] should be(None)
    l.cast[List[List[String]]] should be(None)
  }

  test("test SHAPELESS LIB section Type Safe Cast 1") {
    val `List[String]` = TypeCase[List[String]]
    val `List[Int]`    = TypeCase[List[Int]]
    val l              = List(1, 2, 3)

    val result = (l: Any) match {
      case `List[String]`(List(s, _*)) => s.length
      case `List[Int]`(List(i, _*))    => i + 1
    }

    result should be(2)
  }

}
