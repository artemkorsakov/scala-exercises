package org.scala_exercises.monocle

import monocle.Prism
import org.scala_exercises.monocle.PrismHelper._
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

object PrismHelper {
  sealed trait Json
  case object JNull                     extends Json
  case class JStr(v: String)            extends Json
  case class JNum(v: Double)            extends Json
  case class JObj(v: Map[String, Json]) extends Json

  val jStr = Prism.partial[Json, String] { case JStr(v) => v }(JStr)

  import monocle.std.double.doubleToInt // Prism[Double, Int] defined in Monocle

  val jNum: Prism[Json, Double] = Prism.partial[Json, Double] { case JNum(v) => v }(JNum)

  val jInt: Prism[Json, Int] = jNum composePrism doubleToInt

  import monocle.macros.GenPrism
  val rawJNum: Prism[Json, JNum] = GenPrism[Json, JNum]
}

class PrismTestSuite extends AnyFunSuiteLike with Matchers {
  test("test MONOCLE LIB section Prism 0") {
    jStr("hello") should be(JStr("hello"))
    jStr.getOption(JStr("Hello")) should be(Some("Hello"))
    jStr.getOption(JNum(3.2)) should be(None)
  }

  test("test MONOCLE LIB section Prism 1") {
    jStr.set("Bar")(JStr("Hello")) should be(JStr("Bar"))
    jStr.modify(_.reverse)(JStr("Hello")) should be(JStr("olleH"))
  }

  test("test MONOCLE LIB section Prism 2") {
    jStr.set("Bar")(JNum(10)) should be(JNum(10.0))
    jStr.modify(_.reverse)(JNum(10)) should be(JNum(10))
  }

  test("test MONOCLE LIB section Prism 3") {
    jStr.modifyOption(_.reverse)(JStr("Hello")) should be(Some(JStr("olleH")))
    jStr.modifyOption(_.reverse)(JNum(10)) should be(None)
  }

  test("test MONOCLE LIB section Prism 4") {
    jInt(5) should be(JNum(5.0))
    jInt.getOption(JNum(5.0)) should be(Some(5))
    jInt.getOption(JNum(5.2)) should be(None)
    jInt.getOption(JStr("Hello")) should be(None)
  }

  test("test MONOCLE LIB section Prism 5") {
    rawJNum.getOption(JNum(4.5)) should be(Some(JNum(4.5)))
    rawJNum.getOption(JStr("Hello")) should be(None)
  }

  test("test MONOCLE LIB section Prism 6") {
    val jStr = Prism.partial[Json, String] { case JStr(v) => v }(JStr)

    def partialRoundTripOneWay[S, A](p: Prism[S, A], s: S): Boolean =
      p.getOption(s) match {
        case None    => true // nothing to prove
        case Some(a) => p.reverseGet(a) == s
      }

    def partialRoundTripOtherWay[S, A](p: Prism[S, A], a: A): Boolean =
      p.getOption(p.reverseGet(a)) == Some(a)

    partialRoundTripOneWay(jStr, JStr("Hi")) should be(true)
    partialRoundTripOtherWay(jStr, "Hi") should be(true)
  }

}
