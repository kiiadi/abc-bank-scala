package com.abc

import org.scalatest.{WordSpecLike, Matchers}

/**
 * Unit test of account types.
 */
class AccountTest extends WordSpecLike with Matchers { outer =>

  import Account._

  val AccountId = "testAccountId"
  val CustomerId = "testCustomerId"
  val dateProvider = new DefaultDateProvider

  def newCheckingAccount: CheckingAccount = {
    new CheckingAccount {
      override implicit def dateProvider: DateProvider = outer.dateProvider
      override def id: String = AccountId
    }
  }

  def newSavingsAccount: SavingsAccount = {
    new SavingsAccount {
      override implicit def dateProvider: DateProvider = outer.dateProvider
      override def id: String = AccountId
    }
  }

  def newMaxiSavingsAccount: MaxiSavingsAccount = {
    new MaxiSavingsAccount {
      override implicit def dateProvider: DateProvider = outer.dateProvider
      override def id: String = AccountId
    }
  }

  "A Checking Account" must {

    "deposit 1000" in {

      val account = newCheckingAccount
      account.makeDeposit(1000D) should be(true)
      account.getLastValidation should be(None)
      account.balance should be(1000D)
      account.accountType should be(Checking)
    }

    "fail to deposit amount less than zero" in {

      val account = newCheckingAccount
      account.makeDeposit(-1D) should be(false)
      account.getLastValidation should be(Some("Amount must be greater than zero."))
      account.balance should be(0D)
    }

    "deposit 1000 and withdraw 1000" in {

      val account = newCheckingAccount
      account.makeDeposit(1000D) should be(true)
      account.getLastValidation should be(None)
      account.balance should be(1000D)
      account.makeWithdrawal(1000D) should be(true)
      account.getLastValidation should be(None)
      account.balance should be(0D)
    }

    "fail to withdraw with insufficient funds" in {

      val account = newCheckingAccount
      account.makeWithdrawal(1000D) should be(false)
      account.getLastValidation should be(Some("Insufficient funds."))
      account.balance should be(0D)
    }

    "make three deposits and a withdrawal, then show correct balance and account summary" in {

      val account = newCheckingAccount

      account.makeDeposit(1000D) should be(true)
      account.getLastValidation should be(None)
      account.balance should be(1000D)

      account.makeDeposit(500D) should be(true)
      account.getLastValidation should be(None)
      account.balance should be(1500D)

      account.makeDeposit(2000D) should be(true)
      account.getLastValidation should be(None)
      account.balance should be(3500D)

      account.makeWithdrawal(750D) should be(true)
      account.getLastValidation should be(None)
      account.balance should be(2750D)
    }

    "correctly calculate interest on 1000 balance" in {

      val account = newCheckingAccount

      account.makeDeposit(1000D) should be(true)
      account.getLastValidation should be(None)
      account.balance should be(1000D)

      account.getInterestEarned should be(1D)
    }

    "correctly calculate interest on 10000 balance" in {

      val account = newCheckingAccount

      account.makeDeposit(10000D) should be(true)
      account.getLastValidation should be(None)
      account.balance should be(10000D)

      account.getInterestEarned should be(10D)
    }

    "correctly calculate interest on 500 balance" in {

      val account = newCheckingAccount

      account.makeDeposit(500D) should be(true)
      account.getLastValidation should be(None)
      account.balance should be(500D)

      account.getInterestEarned should be(.5D)
    }
  }

  "A Savings Account" must {

    "correctly calculate interest on 1000 balance" in {

      val account = newSavingsAccount

      account.makeDeposit(1000D) should be(true)
      account.getLastValidation should be(None)
      account.balance should be(1000D)
      account.accountType should be(Savings)

      account.getInterestEarned should be(1D)
    }

    "correctly calculate interest on 900 balance" in {

      val account = newSavingsAccount

      account.makeDeposit(900D) should be(true)
      account.getLastValidation should be(None)
      account.balance should be(900D)

      account.getInterestEarned should be(.9D)
    }

    "correctly calculate interest on 1500 balance" in {

      val account = newSavingsAccount

      account.makeDeposit(1500D) should be(true)
      account.getLastValidation should be(None)
      account.balance should be(1500D)

      account.getInterestEarned should be(2D)
    }

    "correctly calculate interest on 2000 balance" in {

      val account = newSavingsAccount

      account.makeDeposit(2000D) should be(true)
      account.getLastValidation should be(None)
      account.balance should be(2000D)

      account.getInterestEarned should be(3D)
    }

    "correctly calculate interest on 2500 balance" in {

      val account = newSavingsAccount

      account.makeDeposit(2500D) should be(true)
      account.getLastValidation should be(None)
      account.balance should be(2500D)

      account.getInterestEarned should be(4D)
    }
  }

  "A MaxiSavings Account" must {

    "correctly calculate interest on 1000 balance" in {

      val account = newMaxiSavingsAccount

      account.makeDeposit(1000D) should be(true)
      account.getLastValidation should be(None)
      account.balance should be(1000D)
      account.accountType should be(MaxiSavings)

      account.getInterestEarned should be(20D)
    }

    "correctly calculate interest on 900 balance" in {

      val account = newMaxiSavingsAccount

      account.makeDeposit(900D) should be(true)
      account.getLastValidation should be(None)
      account.balance should be(900D)

      account.getInterestEarned should be(18D)
    }

    "correctly calculate interest on 1500 balance" in {

      val account = newMaxiSavingsAccount

      account.makeDeposit(1500D) should be(true)
      account.getLastValidation should be(None)
      account.balance should be(1500D)

      account.getInterestEarned should be(45D)
    }

    "correctly calculate interest on 2000 balance" in {

      val account = newMaxiSavingsAccount

      account.makeDeposit(2000D) should be(true)
      account.getLastValidation should be(None)
      account.balance should be(2000D)

      account.getInterestEarned should be(70D)
    }

    "correctly calculate interest on 2500 balance" in {

      val account = newMaxiSavingsAccount

      account.makeDeposit(2500D) should be(true)
      account.getLastValidation should be(None)
      account.balance should be(2500D)

      account.getInterestEarned should be(120D)
    }
  }
}
