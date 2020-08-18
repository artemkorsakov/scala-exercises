package org.scala_exercises.monocle

import monocle.macros.{ GenLens, Lenses }
import org.scala_exercises.monocle.LensHelper._
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

object LensHelper {
  case class Address(streetNumber: Int, streetName: String)

  val address      = Address(10, "High Street")
  val streetNumber = GenLens[Address](_.streetNumber)

  def neighbors(n: Int): List[Int] =
    if (n > 0) List(n - 1, n + 1) else List(n + 1)

  case class Person(name: String, age: Int, address: Address)
  val john = Person("John", 20, address)

  val addressLens = GenLens[Person](_.address)

  @Lenses
  case class Point(x: Int, y: Int)
  val p = Point(5, 3)

  @Lenses("_")
  case class OtherPoint(x: Int, y: Int)
  val op = OtherPoint(5, 3)

}

class LensTestSuite extends AnyFunSuiteLike with Matchers {
  test("test MONOCLE LIB section Lens 0") {
    streetNumber.get(address) should be(10)
    streetNumber.set(5)(address) should be(Address(5, "High Street"))
  }

  test("test MONOCLE LIB section Lens 1") {
    streetNumber.modify(_ + 1)(address) should be(Address(11, "High Street"))
    val n = streetNumber.get(address)
    n should be(10)
    streetNumber.set(n + 1)(address) should be(Address(11, "High Street"))
  }

  test("test MONOCLE LIB section Lens 2") {
    import cats.implicits._ // to get Functor[List] instance
    streetNumber.modifyF(neighbors)(address) should be(List(Address(9, "High Street"), Address(11, "High Street")))
    streetNumber.modifyF(neighbors)(Address(135, "High Street")) should be(
      List(Address(134, "High Street"), Address(136, "High Street"))
    )
  }

  test("test MONOCLE LIB section Lens 3") {
    (addressLens composeLens streetNumber).get(john) should be(10)
    (addressLens composeLens streetNumber).set(2)(john) should be(Person("John", 20, Address(2, "High Street")))
  }

  test("test MONOCLE LIB section Lens 4") {
    val compose = GenLens[Person](_.name).set("Mike") compose GenLens[Person](_.age).modify(_ + 1)
    compose(john) shouldBe Person("Mike", 21, Address(10, "High Street"))
  }

  test("test MONOCLE LIB section Lens 5") {
    import monocle.std.option.some
    import monocle.macros.GenLens

    case class B(c: Int)
    case class A(b: Option[B])

    val c = GenLens[B](_.c)
    val b = GenLens[A](_.b)

    (b composePrism some composeLens c).getOption(A(Some(B(1)))) shouldBe Some(1)
  }

  test("test MONOCLE LIB section Lens 6") {
    GenLens[Person](_.address.streetName).set("Iffley Road")(john) should be(
      Person("John", 20, Address(10, "Iffley Road"))
    )
  }

  test("test MONOCLE LIB section Lens 7") {
    import monocle.macros.Lenses

    @Lenses case class Point(x: Int, y: Int)
    val p = Point(5, 3)

    Point.x.get(p) shouldBe 5
    Point.y.set(0)(p) shouldBe Point(5, 0)
  }

  test("test MONOCLE LIB section Lens 8") {
    @Lenses("_") case class OtherPoint(x: Int, y: Int)
    val op = OtherPoint(5, 3)

    OtherPoint._x.get(op) shouldBe 5
  }

  test("test MONOCLE LIB section Lens 9") {
    /*
    val streetNumber = Lens[Address, Int](_.streetNumber)(n => a => a.copy(streetNumber = n))

    def getSet[S, A](l: Lens[S, A], s: S): Boolean =
      l.set(l.get(s))(s) == s

    def setGet[S, A](l: Lens[S, A], s: S, a: A): Boolean =
      l.get(l.set(a)(s)) == a

    getSet(streetNumber, address) should be(true)
    setGet(streetNumber, address, 20) should be(true)
   */
  }

}
