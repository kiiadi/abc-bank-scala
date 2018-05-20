package com.abc


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

  val getNewId = java.util.UUID.randomUUID.toString
  def negativeAmountException(amount: Double) = new IllegalArgumentException(s"Amount ${amount} must be greater than zero")
  def exceedsBalanceExeption(amount: Double ) = new IllegalArgumentException(s"Amount ${amount} exceeds balance")
}

sealed trait Account {


  val accountId = Account.getNewId
  val transactions: ListBuffer[Transaction] = ListBuffer[Transaction]()
  private var balance: Double = 0.0


  def getBalance = accountId.synchronized { balance }


  def deposit(amount: Double): Unit = {
    accountId.synchronized {
      Condition.check((amount <= 0), Account.negativeAmountException(amount))
      transactions += Deposit(amount)
      recomputeBalance
    }
  }

  def withdraw(amount: Double) {
    accountId.synchronized {
      Condition.check((amount <= 0), Account.negativeAmountException(amount))
      transactions += Withdrawl(-amount)
      recomputeBalance
    }
  }

  def transferTo(other: Account, amount: Double): Unit = {
    accountId.synchronized {
      Condition.check((amount <= 0), Account.negativeAmountException(amount))
      Condition.check((amount > balance), Account.exceedsBalanceExeption(amount))
      other.accountId.synchronized {
        transactions += TransferFrom(-amount, accountId, other.accountId)
        recomputeBalance
        other.transactions += TransferTo(amount, accountId, other.accountId)
        other.recomputeBalance
      }
    }
  }



  def statementForAccount: String = {
    val transactionSummary = transactions.map(t => t.transactionType + " " + FormatUtils.toDollars(t.amount.abs))
      .mkString("  ", "\n  ", "\n")
    val totalSummary = s"Total ${FormatUtils.toDollars(transactions.map(_.amount).sum)}"
    statementType + "\n" + transactionSummary + totalSummary
  }

  def sumTransactions: Double = accountId.synchronized { transactions.map(_.amount).sum }
  def recomputeBalance: Unit = {
    accountId.synchronized { balance = sumTransactions }
  }
  def interestEarned: Double = interestEarned(getBalance)


   protected[this] def interestEarned(amount: Double): Double = {
      Condition.check((amount <= 0), Account.negativeAmountException(amount))
      amount * Account.CheckingInterest
   }

  def statementType: String

}

case class CheckingAccount() extends Account {

  override def statementType: String = "Checking Account"
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
  override def statementType: String = "Savings Account"
}


case class MaxiSavingsAccount() extends Account {
  def daysSinceLastWithdrawl: Int = 10

  override protected[this] def interestEarned(amount: Double): Double = {
    accountId.synchronized {
      Condition.check((amount <= 0), Account.negativeAmountException(amount))
      if (amount <= Account.MaxiThreshold1) amount * Account.MaxiRate1
      else if (amount <= Account.MaxiThreshold2) (Account.MaxiThreshold1 * Account.MaxiRate1) + (amount - Account.MaxiThreshold1) * Account.MaxiRate2
      else {
        val interestTier1 = Account.MaxiThreshold1 * Account.MaxiRate1
        val interestTier2 = (Account.MaxiThreshold2 - Account.MaxiThreshold1) * Account.MaxiRate2
        val interestTier3 = (amount - Account.MaxiThreshold2) * Account.MaxiRate3
        interestTier1 + interestTier2 + interestTier3
      }
    }
  }

  override def statementType: String = "MaxiSavings Account"
}