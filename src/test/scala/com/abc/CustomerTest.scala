package com.abc

import org.scalatest.{Matchers, FlatSpec}

class CustomerTest extends FlatSpec with Matchers {

  "Customer" should "open one account" in {
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
