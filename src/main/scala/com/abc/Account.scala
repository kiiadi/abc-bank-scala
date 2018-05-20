package com.abc


import java.util.Date

import scala.collection.mutable.ListBuffer

object Account {
  val CheckingInterest = 0.001
  val SavigsInterest = 0.002
  val MaxiRate1 = 0.02
  val MaxiRate2 = 0.05
  val MaxiRate3 = 0.10
  val checkingThreshold = 1000
  val MaxiThreshold1 = 1000
  val MaxiThreshold2 = 2000
  val MaxiSavingsWithdrawlDays = 10

  def negativeAmountException(amount: Double) = new IllegalArgumentException(s"Amount ${amount} must be greater than zero")
  def exceedsBalanceExeption(amount: Double ) = new IllegalArgumentException(s"Amount ${amount} exceeds balance")
}

object AccountStatementType extends Enumeration {
  type statementType = Value
  val Checking_Account , Savings_Account, MaxiSavings_Account = Value
}
sealed trait Account {

  val accountId = FormatUtils.getNewId
  val transactions: ListBuffer[Transaction] = ListBuffer[Transaction]()
  private var balance: Double = 0.0


  def getBalance = accountId.synchronized { balance }


  def deposit(amount: Double, txDate: Date = DateUtils.now): Account = {
    accountId.synchronized {
      Condition.check((amount <= 0), Account.negativeAmountException(amount))
      transactions += Deposit(amount, txDate)
      recomputeBalance
    }
    this
  }

  def withdraw(amount: Double, txDate: Date = DateUtils.now): Account = {
    accountId.synchronized {
      Condition.check((amount <= 0), Account.negativeAmountException(amount))
      transactions += Withdraw(-amount, txDate)
      recomputeBalance
    }
    this
  }

  def transferTo(other: Account, amount: Double, txDate: Date = DateUtils.now): Account = {
    accountId.synchronized {
      Condition.check((amount <= 0), Account.negativeAmountException(amount))
      Condition.check((amount > balance), Account.exceedsBalanceExeption(amount))
      other.accountId.synchronized {
        transactions += TransferFrom(-amount, accountId, other.accountId, txDate)
        recomputeBalance
        other.transactions += TransferTo(amount, accountId, other.accountId, txDate)
        other.recomputeBalance
      }
    }
    this
  }

  def statementForAccount: String = {
    val transactionSummary = transactions.map(t => {
      val tranDollars = FormatUtils.toDollars(t.amount.abs)
      s"${t.transactionType} ${tranDollars}"}
    ).mkString("  ", "\n  ", "\n")

    val totalDollars = FormatUtils.toDollars(sumTransactions)
    val totalSummary = s"Total ${totalDollars}"
    statementType + "\n" + transactionSummary + totalSummary
  }

  def interestEarned: Double = interestEarned(getBalance)
  def sumTransactions: Double = accountId.synchronized { transactions.map(_.amount).sum }
  protected def recomputeBalance: Unit = {
    accountId.synchronized { balance = sumTransactions }
  }

  /**
    * Virtural Method, defaults to Checking Account Rate
    */
  protected[this] def interestEarned(amount: Double): Double = {
    Condition.check((amount <= 0), Account.negativeAmountException(amount))
    amount * Account.CheckingInterest
  }

  // Pure Virtual implemented by Acccount Type
  def statementType: AccountStatementType.statementType

}

case class CheckingAccount() extends Account {

  override def statementType = AccountStatementType.Checking_Account
}

case class SavingsAccount() extends Account {

  override protected[this] def interestEarned(amount: Double): Double = {
    Condition.check((amount <= 0), Account.negativeAmountException(amount))
    if (amount < Account.checkingThreshold) super.interestEarned(amount)
    else {
      val checkingInterest = super.interestEarned(Account.checkingThreshold)
      val savingsInterest = (amount- Account.checkingThreshold) * Account.SavigsInterest
      checkingInterest + savingsInterest
      }
  }
  override def statementType = AccountStatementType.Savings_Account
}


case class MaxiSavingsAccount() extends Account {

  override protected[this] def interestEarned(amount: Double): Double = {
    accountId.synchronized {
      Condition.check((amount <= 0), Account.negativeAmountException(amount))
      if (isWithdrawlWithingDays(Account.MaxiSavingsWithdrawlDays)) super.interestEarned(amount)
      else if (amount <= Account.MaxiThreshold1) amount * Account.MaxiRate1
      else if (amount <= Account.MaxiThreshold2) (Account.MaxiThreshold1 * Account.MaxiRate1) + (amount - Account.MaxiThreshold1) * Account.MaxiRate2
      else {
        val interestTier1 = Account.MaxiThreshold1 * Account.MaxiRate1
        val interestTier2 = (Account.MaxiThreshold2 - Account.MaxiThreshold1) * Account.MaxiRate2
        val interestTier3 = (amount - Account.MaxiThreshold2) * Account.MaxiRate3
        interestTier1 + interestTier2 + interestTier3
      }
    }
  }

  override def statementType = AccountStatementType.MaxiSavings_Account

  def isWithdrawlWithingDays(days: Int): Boolean = {
    val withdrawls = transactions.filter(t => (!(t.isInstanceOf[Deposit] || t.isInstanceOf[TransferTo]))
      && DateUtils.getDaysDiff(t.transactionDate, DateUtils.now) < days)
    (withdrawls.size > 0)
  }
}