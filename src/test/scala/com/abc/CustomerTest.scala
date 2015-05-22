package com.abc

import org.scalatest.{Matchers, FlatSpec}

class CustomerTest extends FlatSpec with Matchers {
  "Customer" should "statement" in {
    val checkingAccount = new Account(Account.AccountType.CHECKING)
    val savingsAccount = new Account(Account.AccountType.SAVINGS)
    val henry = new Customer("Henry").openAccount(checkingAccount).openAccount(savingsAccount)
    checkingAccount.deposit(100.0)
    savingsAccount.deposit(4000.0)
    savingsAccount.withdraw(200.0)
    henry.getStatement should be("Statement for Henry\n" +
      "\nChecking Account\n  deposit $100.00\nTotal $100.00\n" +
      "\nSavings Account\n  deposit $4000.00\n  withdrawal $200.00\nTotal $3800.00\n" +
      "\nTotal In All Accounts $3900.00")
  }

  it should "testOneAccount" in {
    val oscar = new Customer("Oscar").openAccount(new Account(Account.AccountType.SAVINGS))
    oscar.numberOfAccounts should be(1)
  }

  it should "testTwoAccount" in {
    val oscar = new Customer("Oscar").openAccount(new Account(Account.AccountType.SAVINGS))
    oscar.openAccount(new Account(Account.AccountType.CHECKING))
    oscar.numberOfAccounts should be(2)
  }

  ignore should "testThreeAcounts" in {
    val oscar = new Customer("Oscar").openAccount(new Account(Account.AccountType.SAVINGS))
    oscar.openAccount(new Account(Account.AccountType.CHECKING))
    oscar.numberOfAccounts should be(3)
  }
}
