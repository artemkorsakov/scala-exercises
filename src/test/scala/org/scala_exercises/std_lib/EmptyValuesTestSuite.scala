package org.scala_exercises.std_lib

import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

class EmptyValuesTestSuite extends AnyFunSuiteLike with Matchers {
  test("test STD LIB section EmptyValues 0") {
    List() === Nil shouldBe true

    None === None shouldBe true

    None eq None shouldBe true

    assert(None.toString === "None")

    None.toList === Nil shouldBe true

    assert(None.isEmpty === true)

    None.asInstanceOf[Any] === None shouldBe true
    None.asInstanceOf[AnyRef] === None shouldBe true
    None.asInstanceOf[AnyVal] === None shouldBe true

    val optional: Option[String] = None
    assert(optional.isEmpty === true)
    assert(optional === None)
  }

  test("test STD LIB section EmptyValues 1") {
    val optional: Option[String] = Some("Some Value")
    assert((optional == None) === false, "Some(value) should not equal None")
    assert(optional.isEmpty === false, "Some(value) should not be empty")
  }

  test("test STD LIB section EmptyValues 2") {
    val optional: Option[String]  = Some("Some Value")
    val optional2: Option[String] = None
    assert(optional.getOrElse("No Value") === "Some Value", "Should return the value in the option")
    assert(optional2.getOrElse("No Value") === "No Value", "Should return the specified default value")
  }

}
