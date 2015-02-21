package com.abc

import com.abc.account.{AccountType, Account}
import org.scalatest.{Matchers, FlatSpec}

class BankTest extends FlatSpec with Matchers {
  val bank = Bank
  "Bank" should "customer summary" in {
    bank.initialize

    var john = new Customer("John").openAccount(bank.createAccount(AccountType.CHECKING))
    val bill = new Customer("Bill").openAccount(bank.createAccount(AccountType.SAVINGS))
    bank.addCustomer(john)
    bank.addCustomer(bill)
    println(bank.customerSummary)
    bank.customerSummary should be("Customer Summary\n - John (1 account)\n - Bill (1 account)\n")
  }

  it should "checking account" in {
    bank.initialize

    val checkingAccount = bank.createAccount(AccountType.CHECKING)
    val bill = new Customer("Bill").openAccount(checkingAccount)
    bank.addCustomer(bill)
    bill.deposit(AccountType.CHECKING, 100.0)
    bank.totalInterestPaid should be(0.1)
  }

  it should "savings account" in {
    bank.initialize

    val savingsAccount: Account = bank.createAccount(AccountType.SAVINGS)
    val bill = new Customer("Bill").openAccount(savingsAccount)
    bank.addCustomer(bill)
    bill.deposit(AccountType.SAVINGS, 1500.0)
    bank.totalInterestPaid should be(2.0)
  }

  it should "maxi savings account" in {
    bank.initialize

    val maxiSavingsAccount = bank.createAccount(AccountType.MAXI_SAVINGS)
    val bill = new Customer("Bill").openAccount(maxiSavingsAccount)
    bank.addCustomer(bill)
    bill.deposit(AccountType.MAXI_SAVINGS, 3000.0)
    bank.totalInterestPaid should be(150.0)
  }

}
