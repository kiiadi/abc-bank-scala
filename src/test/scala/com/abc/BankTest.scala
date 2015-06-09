package com.abc

import org.scalatest.{WordSpec, Matchers}
import com.abc.AccountTypes._

class BankTest extends WordSpec with Matchers {

  "Bank" when {
    "producing customer summaries" should {
      "for a simple summary" in {
        val bank = new Bank
        val john = new Customer("John")
        john.openAccount(Checking)
        bank.addCustomer(john)
        bank.customerSummary should be(
          """Customer Summary
            | - John (1 account)""".stripMargin)
      }

      "for many customers" in {
        val bank = new Bank

        val john = new Customer("John")
        john.openAccount(Checking)
        bank.addCustomer(john)

        val adeline = new Customer("Adeline")
        adeline.openAccount(Checking)
        adeline.openAccount(Savings)
        bank.addCustomer(adeline)

        val betty = new Customer("Betty")
        betty.openAccount(MaxiSavings)
        betty.openAccount(Checking)
        betty.openAccount(Savings)
        bank.addCustomer(betty)

        bank.customerSummary should be(
          """Customer Summary
            | - John (1 account)
            | - Adeline (2 accounts)
            | - Betty (3 accounts)""".stripMargin)
      }
    }

    "calculating total interest paid" should {
      "for a checking account" in {
        val bank = new Bank
        val bill = new Customer("Bill")
        val checkingAccount = bill.openAccount(Checking)
        bank.addCustomer(bill)
        checkingAccount.deposit(100.0)
        bank.totalInterestPaid should be(0.1)
      }

      "for 2 checking accounts, a savings account and a maxi-savings account" in {
        val bank = new Bank

        val bill = new Customer("Bill")
        val savingsAccount = bill.openAccount(Savings)
        val billsCheckingAccount = bill.openAccount(Checking)
        val maxiSavingsAccount = bill.openAccount(MaxiSavings)
        bank.addCustomer(bill)

        val carrie = new Customer("Carrie")
        val carriesCheckingAccount = carrie.openAccount(Checking)
        bank.addCustomer(carrie)

        carriesCheckingAccount.deposit(1000.0)
        billsCheckingAccount.deposit(100.0)
        savingsAccount.deposit(500.0)
        maxiSavingsAccount.deposit(500.0)

        bank.totalInterestPaid should be(11.6)
      }
    }
  }
}
