package com.abc

import org.scalatest.{Matchers, WordSpec}

class AccountTest extends WordSpec with Matchers {
  "Checking Account" when {
    commonMethods("Checking Account", new CheckingAccount)

    "earning interest" should {
      "earn an interest rate of 0.1%" in {
        val account = new CheckingAccount
        account.deposit(100.0)
        account.interestEarned should be(0.1)
      }
    }
  }

  "Savings Account" when {
    commonMethods("Savings Account", new SavingsAccount)

    "earning interest" should {
      "earn an interest rate of 0.1% for an account under $1000" in {
        val account = new SavingsAccount
        account.deposit(100.0)
        account.interestEarned should be(0.1)
      }

      "earn an interest rate of 0.1% for the first $1000 and then 0.2% for an account over $1000" in {
        val account = new SavingsAccount
        account.deposit(1500.0)
        account.interestEarned should be(2.0)
      }
    }
  }

  "Maxi Savings Account" when {
    commonMethods("Maxi Savings Account", new MaxiSavingsAccount)

    "earn an interest rate of 2% for an account under $1000" in {
      val account = new MaxiSavingsAccount
      account.deposit(500.0)
      account.interestEarned should be(10.0)
    }

    "earn an interest rate of 2% for the first $1000 and 5% for the next $1000 for an account between $1000 and $2000" in {
      val account = new MaxiSavingsAccount
      account.deposit(1500.0)
      account.interestEarned should be(45.0)
    }

    "earn an interest rate of 2% for the first $1000 and 5% for the next $100 and then 10% for the rest for an account over $2000" in {
      val account = new MaxiSavingsAccount
      account.deposit(3000.0)
      account.interestEarned should be(170.0)
    }
  }

  def commonMethods(accountType: String, accountCreator: => Account) {
    "getting a statement" should {
      "with a deposit" in {
        val account = accountCreator
        account.deposit(50.0)
        account.getStatement should be(
          accountType +
            """
              |  deposit $50.00
              |Total $50.00""".stripMargin)
      }

      "with deposits and withdrawals" in {
        val account = accountCreator

        account.deposit(100.0)
        account.deposit(4000.0)
        account.withdraw(200.0)
        account.getStatement should be(
          accountType +
            """
              |  deposit $100.00
              |  deposit $4000.00
              |  withdrawal $200.00
              |Total $3900.00"""
              .stripMargin)
      }
    }
  }
}
