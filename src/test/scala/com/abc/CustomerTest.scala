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

  it should "testOneAccount" in {
    val oscar = new Customer("Oscar").openAccount(new Account(Account.SAVINGS))
    oscar.numberOfAccounts should be(1)
  }

  it should "testTwoAccount" in {
    val oscar = new Customer("Oscar").openAccount(new Account(Account.SAVINGS))
    oscar.openAccount(new Account(Account.CHECKING))
    oscar.numberOfAccounts should be(2)
  }

  it should "testThreeAccounts" in {
    val oscar = new Customer("Oscar").openAccount(new Account(Account.SAVINGS))
    oscar.openAccount(new Account(Account.CHECKING))
    oscar.openAccount(new Account(Account.MAXI_SAVINGS))
    oscar.numberOfAccounts should be(3)
  }
}
