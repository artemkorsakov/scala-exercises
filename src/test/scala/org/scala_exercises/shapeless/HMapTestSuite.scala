package org.scala_exercises.shapeless

import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers
import shapeless._

class HMapTestSuite extends AnyFunSuiteLike with Matchers {
  test("test SHAPELESS LIB section HMap 0") {
    class BiMapIS[K, V]
    implicit val intToString = new BiMapIS[Int, String]
    implicit val stringToInt = new BiMapIS[String, Int]

    val hm = HMap[BiMapIS](23 -> "foo", "bar" -> 13)
    //val hm2 = HMap[BiMapIS](23 -> "foo", 23 -> 13)   // Does not compile

    hm.get(23) should be(Some("foo"))
    hm.get("bar") should be(Some(13))

    import hm._
    val l = 23 :: "bar" :: HNil
    val m = l map hm
    m should be("foo" :: 13 :: HNil)
  }

}
