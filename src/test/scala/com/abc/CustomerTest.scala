package com.abc

import com.abc.account.{AccountType, Account}
import org.scalatest.{Matchers, FlatSpec}

class CustomerTest extends FlatSpec with Matchers {
  val bank = Bank
  "Customer" should "statement" in {
    val checkingAccount = bank.createAccount(AccountType.CHECKING)
    val savingsAccount = bank.createAccount(AccountType.SAVINGS)
    val henry = new Customer("Henry").openAccount(checkingAccount).openAccount(savingsAccount)
    henry.deposit(AccountType.CHECKING, 100.0)
    henry.deposit(AccountType.SAVINGS, 4000.0)
    henry.withdraw(AccountType.SAVINGS, 200.0)
    henry.getStatement should be("Statement for Henry\n" +
      "\nChecking Account\n  deposit $100.00\nTotal $100.00\n" +
      "\nSavings Account\n  deposit $4000.00\n  withdrawal $200.00\nTotal $3800.00\n" +
      "\nTotal In All Accounts $3900.00")
  }

  it should "testOneAccount" in {
    val oscar = new Customer("Oscar").openAccount(bank.createAccount(AccountType.SAVINGS))
    oscar.numberOfAccounts should be(1)
  }

  it should "testTwoAccount" in {
    val oscar = new Customer("Oscar").openAccount(bank.createAccount(AccountType.SAVINGS))
    oscar.openAccount(bank.createAccount(AccountType.CHECKING))
    oscar.numberOfAccounts should be(2)
  }

  it should "testThreeAcounts" in {
    val oscar = new Customer("Oscar").openAccount(bank.createAccount(AccountType.SAVINGS))
    oscar.openAccount(bank.createAccount(AccountType.CHECKING))
    oscar.openAccount(bank.createAccount(AccountType.MAXI_SAVINGS))
    oscar.numberOfAccounts should be(3)
  }

  it should "trasneferBetweemTwoAccounts" in {
    val checkingAccount = bank.createAccount(AccountType.CHECKING)
    val savingsAccount = bank.createAccount(AccountType.SAVINGS)
    val henry = new Customer("Henry").openAccount(checkingAccount).openAccount(savingsAccount)
    henry.deposit(AccountType.CHECKING, 100.0)
    henry.deposit(AccountType.SAVINGS, 4000.0)
    val transferStatus = henry.transfer(AccountType.SAVINGS, AccountType.CHECKING, 3000)
    transferStatus should be (true)
    henry.getStatement should be("Statement for Henry\n" +
      "\nChecking Account\n  deposit $100.00\n  deposit $3000.00\nTotal $3100.00\n" +
      "\nSavings Account\n  deposit $4000.00\n  withdrawal $3000.00\nTotal $1000.00\n" +
      "\nTotal In All Accounts $4100.00")
  }
}
