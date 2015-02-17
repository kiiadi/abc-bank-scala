package com.abc

import org.scalatest.{ Matchers, FlatSpec }
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import scala.util.Try

@RunWith(classOf[JUnitRunner])
class AccountTest extends FlatSpec with Matchers {

  "Account" should "type" in {
    val checkingAccount: Account = new Account(Account.CHECKING)
    checkingAccount.isInstanceOf[Account] should be(true)
  }

  it should "testInvalidDeposit" in {
    val checkingAccount: Account = new Account(Account.CHECKING)

    val message = checkingAccount.deposit(-1).fold(
      (FailedOperation) => "amount must be greater than zero",
      (SuccessOperation) => "")

    message should be("amount must be greater than zero")

  }

  it should "testInvalidWithdraw" in {
    val checkingAccount: Account = new Account(Account.CHECKING)

    val message = checkingAccount.withdraw(-1).fold(
      (FailedOperation) => "amount must be greater than zero",
      (SuccessOperation) => "")

    message should be("amount must be greater than zero")

  }
  
  it should "testDeposit" in {
    val checkingAccount: Account = new Account(Account.CHECKING)

    val message = checkingAccount.deposit(100).fold(
      (FailedOperation) => "amount must be greater than zero",
      (SuccessOperation)=> "deposit success" ) 

    message should be("deposit success")
  }
  
  it should "testWithdraw" in {
    val checkingAccount: Account = new Account(Account.CHECKING)

    val message = checkingAccount.withdraw(100).fold(
      (FailedOperation) => "amount must be greater than zero",
      (SuccessOperation)=> "withdrawal success" ) 

    message should be("withdrawal success")
  }
}