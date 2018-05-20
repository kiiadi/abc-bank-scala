package com.abc

import org.scalatest.{Matchers, FlatSpec}

class CustomerTest extends FlatSpec with Matchers {
  "Customer" should "statement" in {
    val checkingAccount: Account = new CheckingAccount
    val savingsAccount: Account = new SavingsAccount
    val henry: Customer = new Customer("Henry").openAccount(checkingAccount).openAccount(savingsAccount)
    checkingAccount.deposit(100.0)
    savingsAccount.deposit(4000.0)
    savingsAccount.withdraw(200.0)
    henry.getStatement should be("Statement for Henry\n" +
      "\nChecking Account\n  deposit $100.00\nTotal $100.00\n" +
      "\nSavings Account\n  deposit $4000.00\n  withdrawal $200.00\nTotal $3800.00\n" +
      "\nTotal In All Accounts $3900.00")
  }

  it should "testOneAccount" in {
    val oscar: Customer = new Customer("Oscar").openAccount(new SavingsAccount)
    oscar.numberOfAccounts should be(1)
  }

  it should "testTwoAccount" in {
    val oscar: Customer = new Customer("Oscar").openAccount(new SavingsAccount)
    oscar.openAccount(new CheckingAccount)
    oscar.numberOfAccounts should be(2)
  }

  it should "testThreeAcounts" in {
    val oscar: Customer = new Customer("Oscar").openAccount(new SavingsAccount)
    oscar.openAccount(new CheckingAccount)
    oscar.openAccount(new MaxiSavingsAccount)
    oscar.numberOfAccounts should be(3)
  }

  it should "transfer" in {
    val checkingAccount: Account = new CheckingAccount
    val savingsAccount: Account = new SavingsAccount
    val george: Customer = new Customer("George").openAccount(checkingAccount).openAccount(savingsAccount)
    checkingAccount.deposit(100.0)
    savingsAccount.deposit(4000.0)
    george.transfer(savingsAccount, checkingAccount, 1000.0)
    savingsAccount.getBalance should be(3000.0)
    checkingAccount.getBalance should be(1100.00)
    george.getStatement should be("Statement for George\n" +
      "\nChecking Account\n  deposit $100.00\n  transferTo $1000.00\nTotal $1100.00\n" +
      "\nSavings Account\n  deposit $4000.00\n  transferFrom $1000.00\nTotal $3000.00\n" +
      "\nTotal In All Accounts $4100.00")

  }
}
