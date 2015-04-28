package com.abc

import org.scalatest.{Matchers, FlatSpec}

class BankTest extends FlatSpec with Matchers {

  "Bank" should "customer summary" in {
    val bank: Bank = new Bank
    var john: Customer = new Customer("John").openAccount(new CheckingAccount)
    bank.addCustomer(john)
    bank.customerSummary should be("Customer Summary\n - John (1 account)")
  }

  it should "checking account" in {
    val bank: Bank = new Bank
    val checkingAccount: Account = new CheckingAccount
    val bill: Customer = new Customer("Bill").openAccount(checkingAccount)
    bank.addCustomer(bill)
    checkingAccount.deposit(100.0)
    bank.totalInterestPaid should be(0.1)
  }

  it should "savings account" in {
    val bank: Bank = new Bank
    val savingAccount: Account = new SavingAccount
    bank.addCustomer(new Customer("Bill").openAccount(savingAccount))
    savingAccount.deposit(1500.0)
    bank.totalInterestPaid should be(2.0)
  }

  it should "maxi savings account without withdrawal in the last 10 days" in {
    val bank: Bank = new Bank
    val maxiAccount: Account = new MaxiSavingAccount
    bank.addCustomer(new Customer("Bill").openAccount(maxiAccount))
    maxiAccount.deposit(3000.0)
    bank.totalInterestPaid should be(150.0)
  }

  it should "maxi savings account with recent withdrawal" in {
    val bank: Bank = new Bank
    val maxiAccount: Account = new MaxiSavingAccount
    bank.addCustomer(new Customer("Bill").openAccount(maxiAccount))
    maxiAccount.deposit(3000.0)
    maxiAccount.withdraw(100.0)

    bank.totalInterestPaid should be(2.9)
  }

}
