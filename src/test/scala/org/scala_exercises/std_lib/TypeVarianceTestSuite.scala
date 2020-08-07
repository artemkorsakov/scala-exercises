package org.scala_exercises.std_lib

import org.scala_exercises.std_lib.TypeVarianceHelper._
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

object TypeVarianceHelper {

  class Fruit
  abstract class Citrus extends Fruit
  class Orange          extends Citrus
  class Tangelo         extends Citrus
  class Apple           extends Fruit
  class Banana          extends Fruit

}

class TypeVarianceTestSuite extends AnyFunSuiteLike with Matchers {
  test("test STD LIB section TypeVariance 0") {
    class MyContainer[A](val a: A)(implicit manifest: scala.reflect.Manifest[A]) {
      def contents: String = manifest.runtimeClass.getSimpleName
    }

    val fruitBasket = new MyContainer(new Orange())
    fruitBasket.contents should be("Orange")
  }

  test("test STD LIB section TypeVariance 1") {
    class MyContainer[A](val a: A)(implicit manifest: scala.reflect.Manifest[A]) {
      def contents: String = manifest.runtimeClass.getSimpleName
    }

    val fruitBasket = new MyContainer[Fruit](new Orange())
    fruitBasket.contents should be("Fruit")
  }

  test("test STD LIB section TypeVariance 2") {
    class MyContainer[A](val a: A)(implicit manifest: scala.reflect.Manifest[A]) {
      def contents: String = manifest.runtimeClass.getSimpleName
    }

    val fruitBasket: MyContainer[Fruit] = new MyContainer(new Orange())
    fruitBasket.contents should be("Fruit")
  }

  test("test STD LIB section TypeVariance 3") {
    class MyContainer[+A](val a: A)(implicit manifest: scala.reflect.Manifest[A]) {
      def contents: String = manifest.runtimeClass.getSimpleName
    }

    val fruitBasket: MyContainer[Fruit] = new MyContainer[Orange](new Orange())
    fruitBasket.contents should be("Orange")
  }

  test("test STD LIB section TypeVariance 4") {
    class MyContainer[+A](val a: A)(implicit manifest: scala.reflect.Manifest[A]) {
      def contents: String = manifest.runtimeClass.getSimpleName
    }

    val fruitBasket: MyContainer[Fruit] = new MyContainer[Orange](new Orange())
    fruitBasket.contents should be("Orange")

    class NavelOrange extends Orange //Creating a subtype to prove a point
    //val navelOrangeBasket: MyContainer[NavelOrange] = new MyContainer[Orange](new Orange()) //Bad!
    //val tangeloBasket: MyContainer[Tangelo] = new MyContainer[Orange](new Orange()) //Bad!
  }

  test("test STD LIB section TypeVariance 5") {
    class MyContainer[-A](a: A)(implicit manifest: scala.reflect.Manifest[A]) { //Can't receive a val because it would be in a covariant position
      def contents: String = manifest.runtimeClass.getSimpleName
    }

    val citrusBasket: MyContainer[Citrus] = new MyContainer[Citrus](new Orange)
    citrusBasket.contents should be("Citrus")
    val orangeBasket: MyContainer[Orange] = new MyContainer[Citrus](new Tangelo)
    orangeBasket.contents should be("Citrus")
    val tangeloBasket: MyContainer[Tangelo] = new MyContainer[Citrus](new Orange)
    tangeloBasket.contents should be("Citrus")
    val bananaBasket: MyContainer[Banana] = new MyContainer[Fruit](new Apple)
    bananaBasket.contents should be("Fruit")
  }

  test("test STD LIB section TypeVariance 6") {
    class MyContainer[A](val a: A)(implicit manifest: scala.reflect.Manifest[A]) {
      def contents: String = manifest.runtimeClass.getSimpleName
    }

    val citrusBasket: MyContainer[Citrus] = new MyContainer[Citrus](new Orange)
    citrusBasket.contents should be("Citrus")
  }

}
