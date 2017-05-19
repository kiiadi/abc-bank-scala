package com.abc

import org.scalatest.{Matchers, FlatSpec}

class CustomerTest extends FlatSpec with Matchers {
  "Customer" should " have 2 acounts after openning account statement" in {
    val john: Customer = new Customer("John")
    val checkingAccount: Account =  john.openAccount(Account.AccountType.SAVING)
    val savingsAccount: Account = john.openAccount(Account.AccountType.CHECKING)
    john.accounts.size should be(2)
  }

  "Customer" should " not open the same account more then once" in {
    val john: Customer = new Customer("John")
    val checkingAccount: Account =  john.openAccount(Account.AccountType.SAVING)
    intercept[RuntimeException]{
      val savingsAccount: Account = john.openAccount(Account.AccountType.SAVING)
    }
    john.accounts.size should be (1)
  }

  "Total balance" should " be ALL the deposit minus the widraws from the account" in {
    val oscar: Customer = new Customer("Oscar")
    val savingAccount: Account = oscar.openAccount(Account.AccountType.SAVING)
    savingAccount.deposit(Some(100.1))
    savingAccount.deposit(Some(200))
    savingAccount.withdraw(Some(250))
    savingAccount.getBalance should be(50.1)
  }

  "Customer" should " not withdraw money more than account balances" in {
    val john: Customer = new Customer("John")
    val checkingAccount: Account =  john.openAccount(Account.AccountType.SAVING)
    checkingAccount.deposit(Some(100.01))
    intercept[IllegalArgumentException]{
      checkingAccount.withdraw(Some(200.01))
    }
    checkingAccount.getBalance should be (100.01)
  }

  "Total transaction" should " be all the transactions for the accounts" in {
    val oscar: Customer = new Customer("Oscar")
    val savingAccount: Account = oscar.openAccount(Account.AccountType.SAVING)
    savingAccount.deposit(Some(100.1))
    savingAccount.deposit(Some(200))
    savingAccount.withdraw(Some(250))
    savingAccount.getTransactions.size should be (3)
  }

}
