package com.abc

import org.scalatest.{Matchers, FlatSpec}

class BankTest extends FlatSpec with Matchers {

  "Bank" should "summary for one customer" in {
    val bank = new Bank
    bank.addCustomer(new Customer("John").openAccount(new Account(Account.CHECKING)))
    bank.customerSummary should be("Customer Summary\n - John (1 account)")
  }

  it should "summary for two customers" in {
    val bank = new Bank
    bank.addCustomer(new Customer("John").openAccount(new Account(Account.CHECKING)))
    bank.addCustomer(new Customer("Bill").openAccount(new Account(Account.CHECKING)))
    bank.customerSummary should be("Customer Summary\n" +
      " - John (1 account)\n" +
      " - Bill (1 account)")
  }

  it should "summary for one customer with two accounts" in {
    val bank = new Bank
    val john = new Customer("John")
    john.openAccount(new Account(Account.CHECKING))
    john.openAccount(new Account(Account.SAVINGS))
    bank.addCustomer(john)
    bank.customerSummary should be("Customer Summary\n - John (2 accounts)")
  }

  it should "total interest paid" in {
    val bill = new Customer("Bill")
    val checkingAccount = new Account(Account.CHECKING)
    checkingAccount.deposit(100.0)
    bill.openAccount(checkingAccount)

    val savingsAccount = new Account(Account.SAVINGS)
    savingsAccount.deposit(1500.0)
    bill.openAccount(savingsAccount)

    val maxiSavingsAccount = new Account(Account.MAXI_SAVINGS)
    maxiSavingsAccount.deposit(3000.0)
    bill.openAccount(maxiSavingsAccount)

    val bank = new Bank
    bank.addCustomer(bill)
    bank.totalInterestPaid should be(172.1)
  }
}
