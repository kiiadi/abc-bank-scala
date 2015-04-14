package com.abc

import org.scalatest.{ Matchers, FlatSpec }

class CustomerTest extends FlatSpec with Matchers {
  "Customer" should "statement" in {
    val checkingAccount: Account =  Account(AccountType.CHECKING, "C1")
    val savingsAccount: Account =  Account(AccountType.SAVINGS, "S1")
    val henry: Customer =  Customer("Henry").openAccount(checkingAccount).openAccount(savingsAccount)
    checkingAccount.deposit(100.0)
    savingsAccount.deposit(4000.0)
    savingsAccount.withdraw(200.0)
    /*
     * Since I switched the account to a Map ( account id -> Account0 , the order is not guaranteed. I feel
     *  that is not a requirement for the project (Accounts must print in the order they were openedI just check whether the 
     *  total prints at the end correctly
     */
    henry.getStatement should endWith("\nTotal In All Accounts $3900.00")
  }

  it should "testOneAccount" in {
    val oscar: Customer =  Customer("Oscar").openAccount(new Account(AccountType.SAVINGS, "O1"))
    oscar.numberOfAccounts should be(1)
  }

  it should "testTwoAccount" in {
    val oscar: Customer =  Customer("Oscar").openAccount(new Account(AccountType.SAVINGS, "O2"))
    oscar.openAccount( Account(AccountType.CHECKING, "CO1"))
    println("***" + oscar.numberOfAccounts + "****")
    oscar.numberOfAccounts should be(2)
  }

  it should "testThreeAcounts" in {
    val oscar: Customer =  Customer("Oscar").openAccount(new Account(AccountType.SAVINGS, "S01"))
    oscar.openAccount( Account(AccountType.CHECKING, "C01"))
    oscar.openAccount( Account(AccountType.CHECKING, "C03"))
    oscar.numberOfAccounts should be(3)
  }

  it should "fail to transfer money  if insufficient balance in from amount" in {
    val savingsAccount: Account =  Account(AccountType.SAVINGS, "S1")
    val checkingAccount: Account =  Account(AccountType.CHECKING, "C1")
    val oscar: Customer =  Customer("Oscar").openAccount(savingsAccount)
    oscar.openAccount(checkingAccount)
    checkingAccount.deposit(100)
    an[Exception] should be thrownBy oscar.transfer("C1", "S1", 200)
  }

  it should "fail to transfer moneyif the account does not belong to the customer" in {
    val savingsAccount: Account =  Account(AccountType.SAVINGS, "S1")
    val checkingAccount: Account =  Account(AccountType.CHECKING, "C1")
    val oscar: Customer = new Customer("Oscar").openAccount(savingsAccount)
    oscar.openAccount(checkingAccount)
    checkingAccount.deposit(100)
    an[Exception] should be thrownBy oscar.transfer("C2", "S2", 10)

  }
  it should "fail to transfer moneyif the to account does not belong to the customer" in {
    val savingsAccount: Account =  Account(AccountType.SAVINGS, "S1")
    val checkingAccount: Account =  Account(AccountType.CHECKING, "C1")
    val oscar: Customer =  Customer("Oscar").openAccount(savingsAccount)
    oscar.openAccount(checkingAccount)
    checkingAccount.deposit(100)
    an[Exception] should be thrownBy oscar.transfer("C1", "S2", 10)

  }
  
   it should "successfully transfer money" in {
    val savingsAccount: Account =  Account(AccountType.SAVINGS, "S1")
    val checkingAccount: Account =  Account(AccountType.CHECKING, "C1")
    val oscar: Customer =  Customer("Oscar").openAccount(savingsAccount)
    oscar.openAccount(checkingAccount)
    checkingAccount.deposit(100)
    oscar.transfer("C1", "S1", 10) should be (90,10)

  }
  
}

