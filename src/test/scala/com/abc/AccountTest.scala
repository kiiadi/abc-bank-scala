package com.abc

import org.scalatest.{FlatSpec, Matchers}

class AccountTest extends FlatSpec with Matchers {

  "Account" should "checking account interest earned" in {
    val checkingAccount = new Checking
    checkingAccount.deposit(100.0)
    checkingAccount.interestEarned should be(0.1)
  }

  it should "savings account interest earned" in {
    val savingsAccount = new Savings
    savingsAccount.deposit(1500.0)
    savingsAccount.interestEarned should be(2.0)
  }

  it should "maxi savings account interest earned" in {
    val maxiSavingsAccount = new MaxiSavings
    maxiSavingsAccount.deposit(3000.0)
    maxiSavingsAccount.interestEarned should be(150.0)
  }

  it should "maxi savings account interest earned with withdrawal" in {
    val maxiSavingsAccount = new MaxiSavings
    maxiSavingsAccount.deposit(4000.0)
    maxiSavingsAccount.withdraw(1000.0)
    maxiSavingsAccount.interestEarned should be(3.0)
  }

}

