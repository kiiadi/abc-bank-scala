package com.abc


import java.util.Date


import scala.collection.mutable.ListBuffer
import scala.math._

object Account {
  val CheckingInterest = 0.001
  val SavigsInterest = 0.002
  val MaxiRate1 = 0.02
  val MaxiRate2 = 0.05
  val MaxiRate3 = 0.10
  val OneYearDays = 365
  val checkingThreshold = 1000
  val MaxiThreshold1 = 1000
  val MaxiThreshold2 = 2000
  val MaxiSavingsWithdrawlDays = 10

  def negativeAmountException(amount: Double) = new IllegalArgumentException(s"Amount ${amount} must be greater than zero")
  def exceedsBalanceExeption(amount: Double ) = new IllegalArgumentException(s"Amount ${amount} exceeds balance")
}

object AccountStatementType extends Enumeration {
  type statementType = Value
  val Checking_Account , Savings_Account, MaxiSavings_Account, MaxiSaving_Account_Adj = Value
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

  def interestEarned: Double = interestEarnedByTransactions
  def sumTransactions: Double = accountId.synchronized { transactions.map(_.amount).sum }
  protected def recomputeBalance: Unit = {
    accountId.synchronized { balance = sumTransactions }
  }

  def interestEarnedByTransactions: Double = {
    var currentBalance = 0.0;
    var currentInterest = 0.0;
    val now = DateUtils.now
    var lastDate = now
    for (t <- transactions) {
      if (currentBalance == 0.0 && DateUtils.getDaysDiff(lastDate, now) == 0) {
        lastDate = t.transactionDate
        currentBalance += t.amount
      } else {

        currentInterest += interestEarned(currentBalance, lastDate, t.transactionDate)
        currentBalance += t.amount
        lastDate = t.transactionDate
      }
    }
    if (lastDate != now && currentBalance != 0.0) {
      currentInterest += interestEarned(currentBalance, lastDate, now)
    }
    currentInterest
  }


  /**
    * Virtural Method, defaults to Checking Account Rate
    */
  protected[this] def interestEarned(amount: Double, startDate:Date, endDate:Date): Double = {
    Condition.check((amount <= 0), Account.negativeAmountException(amount))
    val days = DateUtils.getDaysDiff(startDate, endDate)
    amount * Account.CheckingInterest/Account.OneYearDays * days
  }
  // Pure Virtual implemented by Acccount Type
  def statementType: AccountStatementType.statementType
}

case class CheckingAccount() extends Account {

  override def statementType = AccountStatementType.Checking_Account
}

case class SavingsAccount() extends Account {

  override protected[this] def interestEarned(amount: Double, startDate: Date, endDate: Date): Double = {
    Condition.check((amount <= 0), Account.negativeAmountException(amount))
    val days = DateUtils.getDaysDiff(startDate, endDate)
    if (amount <= Account.checkingThreshold) super.interestEarned(amount, startDate, endDate)
    else {
      val checkingInterest = super.interestEarned(Account.checkingThreshold, startDate, endDate)
      val savingsInterest = (amount- Account.checkingThreshold) * Account.SavigsInterest/Account.OneYearDays * days
      checkingInterest + savingsInterest
    }
  }
  override def statementType = AccountStatementType.Savings_Account
}

case class MaxiSavingsAccount() extends Account {

  /** Original MaxiSavings Account with three Rates and No Adjustment */
  override protected[this] def interestEarned(amount: Double, startDate: Date, endDate: Date): Double = {
    Condition.check((amount <= 0), Account.negativeAmountException(amount))
    val days = DateUtils.getDaysDiff(startDate, endDate)
    if (amount <= Account.MaxiThreshold1) amount * Account.MaxiRate1 / Account.OneYearDays * days
    else if (amount <= Account.MaxiThreshold2) {
      (Account.MaxiThreshold1 * Account.MaxiRate1 / Account.OneYearDays * days) +
        ((amount - Account.MaxiThreshold1) * Account.MaxiRate2 / Account.OneYearDays * days)
    }
    else {
      val interestTier1 = Account.MaxiThreshold1 * Account.MaxiRate1 / Account.OneYearDays * days
      val interestTier2 = (Account.MaxiThreshold2 - Account.MaxiThreshold1) * Account.MaxiRate2 / Account.OneYearDays * days
      val interestTier3 = (amount - Account.MaxiThreshold2) * Account.MaxiRate3 / Account.OneYearDays * days
      interestTier1 + interestTier2 + interestTier3
    }
  }

  override def statementType = AccountStatementType.MaxiSavings_Account
}

case class MaxiSavingsAccountAdj() extends Account {

  /**
    * Adjustable MaxiSavingsAccount, Change Interest Rate to .5 unless a withdraw or transferFrom has happend in the last 10 Days, then drop the rate
    * for 10 days to .001
    */
  override protected[this] def interestEarned(amount: Double, startDate: Date, endDate: Date): Double = {
      Condition.check((amount <= 0), Account.negativeAmountException(amount))
      val days = DateUtils.getDaysDiff(startDate, endDate)
      val withdrawDays = getWithdrawDaysInPeriod(startDate, Account.MaxiSavingsWithdrawlDays)
      if (withdrawDays > days) super.interestEarned(amount, startDate, endDate)
      else if (withdrawDays == 0) Account.MaxiRate2/Account.OneYearDays * amount * days
      else if (amount <= Account.MaxiThreshold1) amount * Account.MaxiRate1/Account.OneYearDays * days
      else {
        val interestTier1 = amount * Account.MaxiRate1/Account.OneYearDays*withdrawDays
        val interestTier2 = amount * Account.MaxiRate2/Account.OneYearDays*(days - withdrawDays)
        interestTier1 + interestTier2
      }
    }

  def getWithdrawDaysInPeriod(startDate: Date, days: Int): Int = {
    val withdraws = transactions.filter(t => (!(t.isInstanceOf[Deposit] || t.isInstanceOf[TransferTo]))
      && DateUtils.getDaysDiff(t.transactionDate, startDate) < days)
    val minDaysFromStart  = withdraws
      .map(t => DateUtils.getDaysDiff(t.transactionDate, startDate).toInt)
      .reduceLeft(_ min _)

    days - minDaysFromStart
  }

  override def statementType = AccountStatementType.MaxiSaving_Account_Adj
}

