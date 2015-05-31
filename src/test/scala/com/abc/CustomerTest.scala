package com.abc

import org.scalatest.{Matchers, FlatSpec}

class CustomerTest extends FlatSpec with Matchers {

  "Customer" should "statement" in {
    val checkingAccount = new Account(Account.CHECKING)
    val savingsAccount = new Account(Account.SAVINGS)
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
    val oscar = new Customer("Oscar").openAccount(new Account(Account.SAVINGS))
    oscar.numberOfAccounts should be(1)
  }

  it should "open two accounts" in {
    val oscar = new Customer("Oscar").openAccount(new Account(Account.SAVINGS))
    oscar.openAccount(new Account(Account.CHECKING))
    oscar.numberOfAccounts should be(2)
  }

  it should "open three accounts" in {
    val oscar = new Customer("Oscar").openAccount(new Account(Account.SAVINGS))
    oscar.openAccount(new Account(Account.CHECKING))
    oscar.openAccount(new Account(Account.MAXI_SAVINGS))
    oscar.numberOfAccounts should be(3)
  }

  it should "total interest earned" in {
    val fred = new Customer("Fred")
    val checkingAccount = new Account(Account.CHECKING)
    fred.openAccount(checkingAccount)
    checkingAccount.deposit(100.0)

    val savingsAccount = new Account(Account.SAVINGS)
    fred.openAccount(savingsAccount)
    savingsAccount.deposit(1500.0)

    fred.totalInterestEarned should be(2.1)
  }

  it should "transfer between accounts" in {
    val fred = new Customer("Fred")
    val checkingAccount = new Account(Account.CHECKING)
    fred.openAccount(checkingAccount)
    checkingAccount.deposit(100.0)

    val savingsAccount = new Account(Account.SAVINGS)
    fred.openAccount(savingsAccount)

    fred.transfer(10.0, checkingAccount, savingsAccount)

    checkingAccount.getBalance should be(90.0)
    savingsAccount.getBalance should be(10.0)
  }

  it should "transfer from invalid account" in {
    val fred = new Customer("Fred")
    val savingsAccount = new Account(Account.SAVINGS)
    fred.openAccount(savingsAccount)
    an [IllegalArgumentException] should be thrownBy
      fred.transfer(50.0, new Account(Account.CHECKING), savingsAccount)
  }

  it should "transfer to invalid account" in {
    val fred = new Customer("Fred")
    val savingsAccount = new Account(Account.SAVINGS)
    fred.openAccount(savingsAccount)
    an [IllegalArgumentException] should be thrownBy
      fred.transfer(50.0, savingsAccount, new Account(Account.CHECKING))
  }
}
