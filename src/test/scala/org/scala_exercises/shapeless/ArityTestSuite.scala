package org.scala_exercises.shapeless

import shapeless._
import syntax.std.function._
import ops.function._
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

class ArityTestSuite extends AnyFunSuiteLike with Matchers {
  test("test SHAPELESS LIB section Arity 0") {
    def applyProduct[P <: Product, F, L <: HList, R](
        p: P
    )(f: F)(implicit gen: Generic.Aux[P, L], fp: FnToProduct.Aux[F, L => R]) =
      f.toProduct(gen.to(p))

    applyProduct((1, 2))((_: Int) + (_: Int)) should be(3)
    applyProduct((1, 2, 3))((_: Int) * (_: Int) * (_: Int)) should be(6)
  }

}
