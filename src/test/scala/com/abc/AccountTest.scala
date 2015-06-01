package com.abc

import java.util.Date

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

  it should "maxi savings account interest earned with withdrawal over 10 days ago" in {
    val future = new DateProvider().now(11)
    val maxiSavingsAccount = new MaxiSavings(new FrozenDateProvider(future))
    maxiSavingsAccount.deposit(4000.0)
    maxiSavingsAccount.withdraw(1000.0)
    maxiSavingsAccount.interestEarned should be(150.0)
  }

}

class FrozenDateProvider(var date: Date) extends DateProvider {
  override def now = date
}
