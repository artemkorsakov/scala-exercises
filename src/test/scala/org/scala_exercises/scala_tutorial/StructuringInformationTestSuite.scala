package org.scala_exercises.scala_tutorial

import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

class StructuringInformationTestSuite extends AnyFunSuiteLike with Matchers {
  test("test Scala Tutorial LIB section StructuringInformation 0") {
    case class Note(name: String, duration: String, octave: Int)
    val c3 = Note("C", "Quarter", 3)
    c3.name shouldBe "C"
    c3.duration shouldBe "Quarter"
    c3.octave shouldBe 3
  }

  test("test Scala Tutorial LIB section StructuringInformation 1") {
    case class Note(name: String, duration: String, octave: Int)
    val c3      = Note("C", "Quarter", 3)
    val otherC3 = Note("C", "Quarter", 3)
    val f3      = Note("F", "Quarter", 3)
    (c3 == otherC3) shouldBe true
    (c3 == f3) shouldBe false
  }

  test("test Scala Tutorial LIB section StructuringInformation 2") {
    sealed trait Duration
    case object Whole   extends Duration
    case object Half    extends Duration
    case object Quarter extends Duration

    def fractionOfWhole(duration: Duration): Double =
      duration match {
        case Whole   => 1.0
        case Half    => 0.5
        case Quarter => 0.25
      }

    fractionOfWhole(Half) shouldBe 0.5
    fractionOfWhole(Quarter) shouldBe 0.25
  }

}
