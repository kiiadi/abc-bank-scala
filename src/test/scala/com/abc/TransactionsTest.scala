package com.abc

import org.scalatest.{Matchers, WordSpec}

class TransactionsTest extends WordSpec with Matchers {
  "Transactions" when {
    "getting a summary" should {
      "for a withdrawal" in {
        val transactions = new Transactions
        transactions += new Transaction(-100.0)
        transactions.getSummary should be(
          """  withdrawal $100.00
            |Total $-100.00""".stripMargin)
      }

      "for a deposit" in {
        val transactions = new Transactions
        transactions += new Transaction(150.0)
        transactions.getSummary should be(
          """  deposit $150.00
            |Total $150.00""".stripMargin)
      }

      "for a withdrawal and deposit" in {
        val transactions = new Transactions
        transactions += new Transaction(-100.0)
        transactions += new Transaction(150.0)
        transactions.getSummary should be(
          """  withdrawal $100.00
            |  deposit $150.00
            |Total $50.00""".stripMargin)
      }
    }

    "calculating the sum" should {
      "for a withdrawal" in {
        val transactions = new Transactions
        transactions += new Transaction(-100.0)
        transactions.sum should be(-100.0)
      }

      "for a deposit" in {
        val transactions = new Transactions
        transactions += new Transaction(150.0)
        transactions.sum should be(150.0)
      }

      "for a withdrawal and deposit" in {
        val transactions = new Transactions
        transactions += new Transaction(-100.0)
        transactions += new Transaction(150.0)
        transactions.sum should be(50.0)
      }
    }
  }
}
