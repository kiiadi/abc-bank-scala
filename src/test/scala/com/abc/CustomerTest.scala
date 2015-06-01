package com.abc

import org.scalatest.{Matchers, FlatSpec}

class CustomerTest extends FlatSpec with Matchers {

  "Customer" should "statement" in {
    val checkingAccount = new Checking
    val savingsAccount = new Savings
    val henry = new Customer("Henry").openAccount(checkingAccount).openAccount(savingsAccount)
    checkingAccount.deposit(100.0)
    savingsAccount.deposit(4000.0)
    savingsAccount.withdraw(200.0)
    henry.getStatement should be(
      "Statement for Henry\n" +
      "\nChecking Account\n" +
      "  deposit $100.00\n" +
      "Total $100.00\n" +
      "\nSavings Account\n" +
      "  deposit $4000.00\n" +
      "  withdrawal $200.00\n" +
      "Total $3800.00\n" +
      "\nTotal In All Accounts $3900.00")
  }

  it should "open one account" in {
    val oscar = new Customer("Oscar").openAccount(new Savings)
    oscar.numberOfAccounts should be(1)
  }

  it should "open two accounts" in {
    val oscar = new Customer("Oscar").openAccount(new Savings)
    oscar.openAccount(new Checking)
    oscar.numberOfAccounts should be(2)
  }

  it should "open three accounts" in {
    val oscar = new Customer("Oscar").openAccount(new Savings)
    oscar.openAccount(new Checking)
    oscar.openAccount(new MaxiSavings)
    oscar.numberOfAccounts should be(3)
  }

  it should "total interest earned" in {
    val fred = new Customer("Fred")
    val checkingAccount = new Checking
    fred.openAccount(checkingAccount)
    checkingAccount.deposit(100.0)

    val savingsAccount = new Savings
    fred.openAccount(savingsAccount)
    savingsAccount.deposit(1500.0)

    fred.totalInterestEarned should be(2.1)
  }

  it should "transfer between accounts" in {
    val fred = new Customer("Fred")
    val checkingAccount = new Checking
    fred.openAccount(checkingAccount)
    checkingAccount.deposit(100.0)

    val savingsAccount = new Savings
    fred.openAccount(savingsAccount)

    fred.transfer(10.0, checkingAccount, savingsAccount)

    checkingAccount.balance should be(90.0)
    savingsAccount.balance should be(10.0)
  }

  it should "transfer from invalid account" in {
    val fred = new Customer("Fred")
    val savingsAccount = new Savings
    fred.openAccount(savingsAccount)
    an [IllegalArgumentException] should be thrownBy
      fred.transfer(50.0, new Checking, savingsAccount)
  }

  it should "transfer to invalid account" in {
    val fred = new Customer("Fred")
    val savingsAccount = new Savings
    fred.openAccount(savingsAccount)
    an [IllegalArgumentException] should be thrownBy
      fred.transfer(50.0, savingsAccount, new Checking)
  }
}
