package org.scala_exercises.cats

import cats._
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

class FunctorTestSuite extends AnyFunSuiteLike with Matchers {
  test("test STD LIB section Functor 0") {
    Functor[List].map(List("qwer", "adsfg"))(_.length) should be(List(4, 5))

    Functor[Option].map(Option("Hello"))(_.length) should be(Some(5))
    Functor[Option].map(None: Option[String])(_.length) should be(None)
  }

  test("test STD LIB section Functor 1") {
    val lenOption: Option[String] => Option[Int] = Functor[Option].lift(_.length)
    lenOption(Some("Hello")) should be(Some(5))
  }

  test("test STD LIB section Functor 2") {
    val source  = List("Cats", "is", "awesome")
    val product = Functor[List].fproduct(source)(_.length).toMap

    product.get("Cats").getOrElse(0) should be(4)
    product.get("is").getOrElse(0) should be(2)
    product.get("awesome").getOrElse(0) should be(7)
  }

  test("test STD LIB section Functor 3") {
    val listOpt = Functor[List] compose Functor[Option]
    listOpt.map(List(Some(1), None, Some(3)))(_ + 1) should be(List(Some(2), None, Some(4)))
  }

}
