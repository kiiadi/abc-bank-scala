package com.abc

import org.scalatest.{FlatSpec, Matchers}

class AccountTest extends FlatSpec with Matchers {

  it should "checking account" in {
    val checkingAccount = new Checking
    checkingAccount.deposit(100.0)
    checkingAccount.interestEarned should be(0.1)
  }

  it should "savings account" in {
    val savingsAccount = new Savings
    savingsAccount.deposit(1500.0)
    savingsAccount.interestEarned should be(2.0)
  }

  it should "maxi savings account" in {
    val maxiSavingsAccount = new MaxiSavings
    maxiSavingsAccount.deposit(3000.0)
    maxiSavingsAccount.interestEarned should be(170.0)
  }

}
