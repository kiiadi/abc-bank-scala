package com.abc

import org.scalatest.{WordSpec, Matchers}
import com.abc.AccountTypes._

class CustomerTest extends WordSpec with Matchers {
  "Customer" when {
    "opening accounts" should {
      "for a savings account" in {
        val oscar = new Customer("Oscar")
        oscar.openAccount(Savings)
        oscar.numberOfAccounts should be(1)
      }

      "for a savings and checking account" in {
        val oscar = new Customer("Oscar")
        oscar.openAccount(Savings)
        oscar.openAccount(Checking)
        oscar.numberOfAccounts should be(2)
      }

      "for a savings, checking and maxi-savings account" in {
        val oscar = new Customer("Oscar")
        oscar.openAccount(Savings)
        oscar.openAccount(Checking)
        oscar.openAccount(MaxiSavings)
        oscar.numberOfAccounts should be(3)
      }
    }

    "getting a statement" should {
      "for a simple statement for 1 account" in {
        val matthew = new Customer("Matthew")
        val checkingAccount = matthew.openAccount(Checking)
        checkingAccount.deposit(50.0)
        matthew.getStatement should be(
          """Statement for Matthew
            |
            |Checking Account
            |  deposit $50.00
            |Total $50.00
            |
            |Total In All Accounts $50.00""".stripMargin)
      }

      "for a statement for 2 accounts" in {
        val henry = new Customer("Henry")
        val checkingAccount = henry.openAccount(Checking)
        val savingsAccount = henry.openAccount(Savings)
        checkingAccount.deposit(100.0)
        savingsAccount.deposit(4000.0)
        savingsAccount.withdraw(200.0)
        henry.getStatement should be(
          """Statement for Henry
            |
            |Checking Account
            |  deposit $100.00
            |Total $100.00
            |
            |Savings Account
            |  deposit $4000.00
            |  withdrawal $200.00
            |Total $3800.00
            |
            |Total In All Accounts $3900.00""".stripMargin)
      }
    }

    "earning interest" should {
      "earn total interest for 2 accounts" in {
        val bill = new Customer("Bill")
        val checkingAccount = bill.openAccount(Checking)
        val savingsAccount = bill.openAccount(Savings)
        checkingAccount.deposit(1000.0)
        savingsAccount.deposit(100.0)
        bill.totalInterestEarned should be(1.1)
      }
    }

    "transferring between accounts" should {
      val bill = new Customer("Bill")
      val checkingAccount = bill.openAccount(Checking)
      checkingAccount.deposit(1000.0)
      val savingsAccount = bill.openAccount(Savings)
      savingsAccount.deposit(1500.00)
      bill.transfer(checkingAccount, savingsAccount, 500.0)

      "have each accounts balance updated" in {
        checkingAccount.sumTransactions should be(500.0)
        savingsAccount.sumTransactions should be(2000.0)
      }

      "record the transactions in each account" in {
        checkingAccount.getStatement should be(
          """Checking Account
            |  deposit $1000.00
            |  withdrawal $500.00
            |Total $500.00""".stripMargin)
        savingsAccount.getStatement should be(
          """Savings Account
            |  deposit $1500.00
            |  deposit $500.00
            |Total $2000.00""".stripMargin)
      }
    }

    "not allow transfers between accounts when the customer does not own those accounts" should {
      val bill = new Customer("Bill")
      val checkingAccount = new CheckingAccount
      val savingsAccount = bill.openAccount(Savings)

      "when the customer doesn't own the from account" in {
        the [IllegalArgumentException] thrownBy {
          bill.transfer(checkingAccount, savingsAccount, 100.0)
        } should have message "Customer must own 'from' account"
      }

      "when the customer doesn't own the to account" in {
        the [IllegalArgumentException] thrownBy {
          bill.transfer(savingsAccount, checkingAccount, 100.0)
        } should have message "Customer must own 'to' account"
      }
    }
  }
}
