package org.scala_exercises.circe

import io.circe._
import circelib.helpers.OpticsHelpers._
import io.circe.optics.JsonPath._
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

class OpticsTestSuite extends AnyFunSuiteLike with Matchers {
  test("test CIRCE LIB section Optics 0") {
    val _address                = root.order.customer.contactDetails.address.string
    val address: Option[String] = _address.getOption(json)
    address should be(Some("1 Fake Street, London, England"))
  }

  test("test CIRCE LIB section Optics 1") {
    val items: List[String] = root.order.items.each.description.string.getAll(json)
    items should be(List("banana", "apple"))
  }

  test("test CIRCE LIB section Optics 2") {
    val doubleQuantities: Json => Json = root.order.items.each.quantity.int.modify(_ * 2)

    val modifiedJson = doubleQuantities(json)

    val modifiedQuantities: List[Int] = root.order.items.each.quantity.int.getAll(modifiedJson)
    modifiedQuantities should be(List(2, 4))
  }

  test("test CIRCE LIB section Optics 3") {
    import io.circe.optics.JsonOptics._
    import monocle.function.Plated

    val recursiveModifiedJson = Plated.transform[Json] { j =>
      j.asNumber match {
        case Some(n) => Json.fromString(n.toString)
        case None    => j
      }
    }(json)

    root.order.total.string.getOption(recursiveModifiedJson) shouldBe Some("123.45")
  }

  test("test CIRCE LIB section Optics 4") {
    val doubleQuantities: Json => Json =
      root.order.itemss.each.quantity.int.modify(_ * 2) // Note the "itemss" typo

    val modifiedJson = doubleQuantities(json)

    val modifiedQuantitiesDynamic: List[Int] =
      root.order.items.each.quantity.int.getAll(modifiedJson)

    modifiedQuantitiesDynamic == List(2, 4) should be(false)
  }

}
