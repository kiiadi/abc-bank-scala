package com.abc

import java.util.Date

import org.scalatest.{FlatSpec, Matchers}


class AccountTest extends FlatSpec with Matchers {

  "Account" should "check exception for deposit" in {
    val acc1: Account = new CheckingAccount
    val thrown = intercept[Exception] {
      acc1.deposit(-1000)
    }
    assert(thrown.getMessage === "Amount -1000.0 must be greater than zero")
  }

  it should "check exception for withdraw" in {
    val acc1: Account = new SavingsAccount
    val thrown = intercept[Exception] {
      acc1.withdraw(-1000)
    }
    assert(thrown.getMessage === "Amount -1000.0 must be greater than zero")
  }

  it should "check exceptions for transfer" in {
    val acc1: Account = new SavingsAccount
    val acc2: Account = new CheckingAccount
    val thrown = intercept[Exception] {
      acc1.transferTo(acc2, -1000)
    }
    assert(thrown.getMessage === "Amount -1000.0 must be greater than zero")

    val thrown2 = intercept[Exception] {
      acc1.transferTo(acc2, 2000)
    }
    assert(thrown2.getMessage === "Amount 2000.0 exceeds balance")
  }

  it should "mix all transactions and check state" in {
    val chk: Account = new CheckingAccount
    val sav: Account = new SavingsAccount
    val max: Account = new MaxiSavingsAccount
    val max2: Account = new MaxiSavingsAccount
    val lastYear: Date = DateUtils.getDaysAgo(365)
    chk.deposit(1000.00, lastYear).withdraw(500.00,lastYear).deposit(1500.00, lastYear)
    sav.deposit(2000.00,lastYear).withdraw(1000.00,lastYear).deposit(500.0, lastYear)
    max.deposit(3000.00,lastYear).withdraw(1000.00, lastYear).transferTo(chk,1000.00,lastYear).deposit(2000, lastYear)
    chk.getBalance should be(3000)
    chk.transactions.size should be(4)
    chk.interestEarned - 3 should be < .001
    sav.getBalance should be(1500)
    sav.transactions.size should be(3)
    sav.interestEarned - 2 should be < .001
    max.getBalance should be(3000)
    max.transactions.size should be(4)
    max.interestEarned should be(170)
  }


}