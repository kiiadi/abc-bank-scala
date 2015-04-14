package com.abc

import org.scalatest.{ Matchers, FlatSpec }
import java.util.Calendar

class AccountTest extends FlatSpec with Matchers {
  "Account" should "fail when we try to deposit negative amount" in {
    val checkingAccount: Account = Account(AccountType.CHECKING, "C1")
    val savingsAccount: Account = Account(AccountType.SAVINGS, "S1")

    an[Exception] should be thrownBy checkingAccount.deposit(-10)
  }

  it should "successfully take positive deposits" in {
    val checkingAccount: Account = Account(AccountType.CHECKING, "C1")
    val savingsAccount: Account = Account(AccountType.SAVINGS, "S1")

    checkingAccount.deposit(10)
    checkingAccount.getTransactionCount should equal(1)
  }

  it should "fail when we withdraw negative amounts" in {
    val checkingAccount: Account = Account(AccountType.CHECKING, "C1")
    val savingsAccount: Account = Account(AccountType.SAVINGS, "S1")

    an[Exception] should be thrownBy checkingAccount.withdraw(-10)

  }

  it should "fail when we withdraw  amounts more than the balance" in {
    val checkingAccount: Account = Account(AccountType.CHECKING, "C1")
    val savingsAccount: Account = Account(AccountType.SAVINGS, "S1")

    an[Exception] should be thrownBy checkingAccount.withdraw(10)

  }

  it should "successfully take withdraw deposits" in {
    val checkingAccount: Account = Account(AccountType.CHECKING, "C1")
    val savingsAccount: Account = Account(AccountType.SAVINGS, "S1")

    checkingAccount.deposit(100)
    checkingAccount.withdraw(50)
    checkingAccount.sumTransactions() should be(50)
  }

  it should "successfully retrieve last withdrwal date from a bunch of transaction " in {
    //Ideally we should use some mock transaction but I am subclassing it to override the date for testing
    var after10Days = Calendar.getInstance
    after10Days.add(Calendar.DAY_OF_MONTH, 10)
    val expectedDate = after10Days.getTime

    val t1 = new TestTransaction(100, Calendar.getInstance.getTime)
    val t2 = new TestTransaction(-10, expectedDate)
    val t3 = new TestTransaction(-20, Calendar.getInstance.getTime)

    import scala.collection.mutable.ListBuffer
    val checkingAccount: Account = Account(AccountType.CHECKING, "C1",0, ListBuffer(t1, t2, t3))
    checkingAccount.lastWithdrawalDate should be(expectedDate)

  }

  it should "successfully calculate the number of days since last withdrawal " in {
    //Ideally we should use some mock transaction but I am subclassing it to override the date for testing
    var after10Days = Calendar.getInstance
    after10Days.add(Calendar.DAY_OF_MONTH, 10)
    val expectedDate = after10Days.getTime

    val t1 = new TestTransaction(100, Calendar.getInstance.getTime)
    val t2 = new TestTransaction(-10, expectedDate)
    val t3 = new TestTransaction(-20, Calendar.getInstance.getTime)

    import scala.collection.mutable.ListBuffer
    val checkingAccount: Account = Account(AccountType.CHECKING, "C1", 0,ListBuffer(t1, t2, t3))
    //The test may be flaky since we have the time component hence I am checking the range
   
    assert (checkingAccount.daysSinceLastWithDrawal  >= 9 && checkingAccount.daysSinceLastWithDrawal  <=  11)
  }

  it should "successfully calculate the account balance" in {
    val checkingAccount: Account = Account(AccountType.CHECKING, "C1")

    checkingAccount.deposit(10)
    checkingAccount.deposit(1000)
    checkingAccount.withdraw(10)

    checkingAccount.sumTransactions() should be(1000)

  }

}

