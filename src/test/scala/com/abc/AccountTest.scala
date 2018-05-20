package com.abc

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
    chk.deposit(1000.00).withdraw(500.00).deposit(1500.00)
    sav.deposit(2000.00).withdraw(1000.00).deposit(500.0)
    max.deposit(3000.00).withdraw(1000.00).transferTo(chk,1000.00)
    max2.deposit(3000)
    chk.getBalance should be(3000)
    chk.transactions.size should be(4)
    chk.interestEarned should be(3.0)
    sav.getBalance should be(1500)
    sav.transactions.size should be(3)
    sav.interestEarned should be(2.0)
    max.getBalance should be(1000)
    max.transactions.size should be(3)
    max.interestEarned should be(1.0)
    max2.interestEarned should be (170)
  }


}