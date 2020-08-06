package org.scala_exercises.std_lib

import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

class TraitsTestSuite extends AnyFunSuiteLike with Matchers {
  test("test STD LIB section Traits 0") {
    case class Event(name: String)

    trait EventListener {
      def listen(event: Event): String
    }

    class MyListener extends EventListener {
      def listen(event: Event): String =
        event match {
          case Event("Moose Stampede") =>
            "An unfortunate moose stampede occurred"
          case _ => "Nothing of importance occurred"
        }
    }

    val evt        = Event("Moose Stampede")
    val myListener = new MyListener
    myListener.listen(evt) should be("An unfortunate moose stampede occurred")
  }

  test("test STD LIB section Traits 1") {
    // A class can only extend from one class or trait, any subsequent extension should use the keyword with:
    case class Event(name: String)

    trait EventListener {
      def listen(event: Event): String
    }

    class OurListener

    class MyListener extends OurListener with EventListener {
      def listen(event: Event): String =
        event match {
          case Event("Woodchuck Stampede") =>
            "An unfortunate woodchuck stampede occurred"
          case _ => "Nothing of importance occurred"
        }
    }

    val evt        = Event("Woodchuck Stampede")
    val myListener = new MyListener
    myListener.listen(evt) should be("An unfortunate woodchuck stampede occurred")
  }

  test("test STD LIB section Traits 2") {
    case class Event(name: String)

    trait EventListener {
      def listen(event: Event): String
    }

    class MyListener extends EventListener {
      def listen(event: Event): String =
        event match {
          case Event("Moose Stampede") =>
            "An unfortunate moose stampede occurred"
          case _ => "Nothing of importance occurred"
        }
    }

    val myListener = new MyListener

    myListener.isInstanceOf[MyListener] should be(true)
    myListener.isInstanceOf[EventListener] should be(true)
    myListener.isInstanceOf[Any] should be(true)
    myListener.isInstanceOf[AnyRef] should be(true)
  }

  test("test STD LIB section Traits 3") {
    trait B {
      def bId = 2
    }

    trait A { self: B =>

      def aId = 1
    }

    //val a = new A  //***does not compile!!!***
    val obj = new A with B
    (obj.aId + obj.bId) should be(3)
  }

}
