package org.scala_exercises.shapeless

import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers
import shapeless._

class CoproductsTestSuite extends AnyFunSuiteLike with Matchers {
  test("test SHAPELESS LIB section Coproducts 0") {
    type ISB = Int :+: String :+: Boolean :+: CNil

    val isb = Coproduct[ISB]("foo")

    isb.select[Int] should be(None)

    isb.select[String] should be(Some("foo"))

    object sizeM extends Poly1 {
      implicit def caseInt     = at[Int](i => (i, i))
      implicit def caseString  = at[String](s => (s, s.length))
      implicit def caseBoolean = at[Boolean](b => (b, 1))
    }

    val m = isb map sizeM

    m.select[(String, Int)] should be(Some(("foo", 3)))
  }

  test("test SHAPELESS LIB section Coproducts 1") {
    import union._, syntax.singleton._

    type U = Union.`'i -> Int, 's -> String, 'b -> Boolean`.T

    val u = Coproduct[U](Symbol("s") ->> "foo") // Inject a String into the union at label 's

    u.get(Symbol("i")) should be(None)
    u.get(Symbol("s")) should be(Some("foo"))
    u.get(Symbol("b")) should be(None)
  }

}
