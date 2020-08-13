package org.scala_exercises.shapeless

import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers
import shapeless._
import syntax.std.tuple._
import syntax.singleton._

class SingletonsLiteralsTestSuite extends AnyFunSuiteLike with Matchers {
  test("test SHAPELESS LIB section Singletons Literals 0") {
    val hlist = 23 :: "foo" :: true :: HNil
    hlist(1) should be("foo")

    val tuple = (23, "foo", true)
    tuple(1) should be("foo")

  }

  test("test SHAPELESS LIB section Singletons Literals 1") {
    Witness(23).value == 23 shouldBe true
  }

  test("test SHAPELESS LIB section Singletons Literals 2") {
    Witness("foo").value == "foo" should be(true)
  }

  test("test SHAPELESS LIB section Singletons Literals 3") {
    val (wTrue, wFalse) = (Witness(true), Witness(false))

    type True  = wTrue.T
    type False = wFalse.T

    trait Select[B] { type Out }

    implicit val selInt    = new Select[True]  { type Out = Int    }
    implicit val selString = new Select[False] { type Out = String }

    def select(b: WitnessWith[Select])(t: b.instance.Out) = t

    select(true)(23) should be(23)

    //select(true)("foo")
    //error: type mismatch;
    // found   : String("foo")
    // required: Int
    //              select(true)("foo")
    //                           ^

    //select(false)(23)
    // error: type mismatch;
    //found   : Int(23)
    //required: String

    select(false)("foo") should be("foo")
  }

}
