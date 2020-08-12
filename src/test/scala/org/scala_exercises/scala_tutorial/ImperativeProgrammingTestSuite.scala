package org.scala_exercises.scala_tutorial

import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

class ImperativeProgrammingTestSuite extends AnyFunSuiteLike with Matchers {
  test("test STD LIB section ImperativeProgramming 0") {
    class BankAccount {
      private var balance = 0
      def deposit(amount: Int): Int = {
        if (amount > 0) balance = balance + amount
        balance
      }
      def withdraw(amount: Int): Int =
        if (0 < amount && amount <= balance) {
          balance = balance - amount
          balance
        } else throw new Error("insufficient funds")
    }

    val x = new BankAccount
    val y = new BankAccount
    x deposit 30
    x withdraw 20 shouldBe 10
  }

  test("test STD LIB section ImperativeProgramming 1") {
    def factorial(n: Int): Int = {
      var result = 1
      var i      = 1
      while (i <= n) {
        result = result * i
        i = i + 1
      }
      result
    }

    factorial(2) shouldBe 2
    factorial(3) shouldBe 6
    factorial(4) shouldBe 24
    factorial(5) shouldBe 120
  }

}
