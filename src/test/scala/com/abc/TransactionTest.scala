package com.abc

import org.scalatest.{WordSpec, Matchers}

class TransactionTest extends WordSpec with Matchers {
  "Transaction" when {
    "getting a statement" should {
      "for a withdrawal" in {
        new Transaction(-100.0).getSummary should be("withdrawal $100.00")
      }

      "for a deposit" in {
        new Transaction(150.0).getSummary should be("deposit $150.00")
      }
    }
  }
}
