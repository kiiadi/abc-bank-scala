package com.abc

import org.scalatest.{Matchers, FlatSpec}

class BankTest extends FlatSpec with Matchers {

  "Bank" should "customer summary" in {
    val bank = new Bank
    val john = new Customer("John").openAccount(new Account(Account.AccountType.CHECKING))
    bank.addCustomer(john)
    bank.customerSummary should be("Customer Summary\n - John (1 account)")

    john.openAccount(new Account(Account.AccountType.SAVINGS))
    bank.customerSummary should be("Customer Summary\n - John (2 accounts)")
  }

  it should "get correct first customer" in {
    val bank = new Bank
    bank.getFirstCustomer should be("Error")

    val john = new Customer("John")
    bank.addCustomer(john)
    bank.getFirstCustomer should be("John")
  }

  it should "checking account" in {
    val bank = new Bank
    val checkingAccount = new Account(Account.AccountType.CHECKING)
    val bill: Customer = new Customer("Bill").openAccount(checkingAccount)
    bank.addCustomer(bill)
    checkingAccount.deposit(100.0)
    bank.totalInterestPaid should be(0.1)
  }

  it should "savings account" in {
    val bank = new Bank
    val checkingAccount = new Account(Account.AccountType.SAVINGS)
    bank.addCustomer(new Customer("Bill").openAccount(checkingAccount))
    checkingAccount.deposit(1000.0)
    bank.totalInterestPaid should be(1.0)

    checkingAccount.deposit(500.0)
    bank.totalInterestPaid should be(2.0)
  }

  it should "maxi savings account" in {
    val bank = new Bank
    val checkingAccount = new Account(Account.AccountType.MAXI_SAVINGS)
    bank.addCustomer(new Customer("Bill").openAccount(checkingAccount))

    checkingAccount.deposit(1000.0)
    bank.totalInterestPaid should be(20.0)

    checkingAccount.deposit(1000.0)
    bank.totalInterestPaid should be(70.0)

    checkingAccount.deposit(1000.0)
    bank.totalInterestPaid should be(170.0)
  }

}
