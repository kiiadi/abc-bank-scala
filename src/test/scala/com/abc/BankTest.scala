package com.abc

import org.scalatest.{Matchers, FlatSpec}

class BankTest extends FlatSpec with Matchers {

  "Bank" should "customer summary" in {
    val bank: Bank = new Bank
    val john: Customer = new Customer("John").openAccount(new Account(Account.CHECKING))
    bank.addCustomer(john)
    bank.customerSummary should be("Customer Summary\n - John (1 account)")
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
