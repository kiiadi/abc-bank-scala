package com.abc

import org.scalatest.{Matchers, FlatSpec}

class BankTest extends FlatSpec with Matchers {

  "Bank" should "summary for one customer" in {
    val bank = new Bank
    bank.addCustomer(new Customer("John").openAccount(new Checking))
    bank.customerSummary should be("Customer Summary\n - John (1 account)")
  }

  it should "summary for two customers" in {
    val bank = new Bank
    bank.addCustomer(new Customer("John").openAccount(new Checking))
    bank.addCustomer(new Customer("Bill").openAccount(new Checking))
    bank.customerSummary should be("Customer Summary\n" +
      " - John (1 account)\n" +
      " - Bill (1 account)")
  }

  it should "summary for one customer with two accounts" in {
    val bank = new Bank
    val john = new Customer("John")
    john.openAccount(new Checking)
    john.openAccount(new Savings)
    bank.addCustomer(john)
    bank.customerSummary should be("Customer Summary\n - John (2 accounts)")
  }

  it should "total interest paid" in {
    val bill = new Customer("Bill")
    val checkingAccount = new Checking
    checkingAccount.deposit(100.0)
    bill.openAccount(checkingAccount)

    val savingsAccount = new Savings
    savingsAccount.deposit(1500.0)
    bill.openAccount(savingsAccount)

    val maxiSavingsAccount = new MaxiSavings
    maxiSavingsAccount.deposit(3000.0)
    bill.openAccount(maxiSavingsAccount)

    val bank = new Bank
    bank.addCustomer(bill)
    bank.totalInterestPaid should be(172.1)
  }
}
