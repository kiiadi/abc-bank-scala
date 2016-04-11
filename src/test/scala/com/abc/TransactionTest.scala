package com.abc

import org.scalatest.{FlatSpec, Matchers}

class TransactionTest extends FlatSpec with Matchers {
    val now = System.currentTimeMillis

    "Deposit" should "not be negative" in {
        intercept[IllegalArgumentException] {
            Transaction.deposit("-5", now)
        }
    }

    "Deposit" should "be positive" in {
        Transaction.deposit("5", now) should be(Transaction.Deposit(5, now))
    }

    "Withdrawal" should "not be negative" in {
        intercept[IllegalArgumentException] {
            Transaction.withdrawal("-5", now)
        }
    }

    "Withdrawal" should "be positive" in {
        Transaction.withdrawal("5", now) should be(Transaction.Withdrawal(5, now))
    }
}
