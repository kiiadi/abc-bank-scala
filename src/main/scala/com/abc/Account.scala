package com.abc
import scala.collection.mutable.ListBuffer

object Account {
  val CHECKING_INT_RATE = 0.001
  val SAVING_LOWER_INT_RATE = 0.001
  val SAVING_UPPER_INT_RATE = 0.002
  val SAVING_BENCH_MARK = 1000
  val MAX_SAVING_LOWER_INT_RATE = 0.002
  val MAX_SAVING_MID_INT_RATE = 0.005
  val MAX_SAVING_UPPER_INT_RATE = 0.005
  val MAX_SAVING_MID_BENCH_MARK = 1000
  val MAX_SAVING_HIGH_BENCH_MARK = 2000

  object AccountType extends Enumeration{
    type accountType = Value
    val CHECKING, SAVING, MAX_SAVING = Value
  }

  def apply (accountType: AccountType.Value, accoutHolder: String) = new Account(accountType,accoutHolder)

  def interestEarned(account: Account): BigDecimal = {
    val amount: BigDecimal = account.transactions.map(_.amount).sum
    account.accountType match {
      case AccountType.SAVING =>
        if (amount <= SAVING_BENCH_MARK) amount * SAVING_LOWER_INT_RATE
        else SAVING_BENCH_MARK * SAVING_LOWER_INT_RATE + (amount - SAVING_BENCH_MARK) * SAVING_UPPER_INT_RATE
      case AccountType.MAX_SAVING =>
        if (amount <= MAX_SAVING_MID_BENCH_MARK)  amount * MAX_SAVING_LOWER_INT_RATE
        if (amount <= MAX_SAVING_HIGH_BENCH_MARK)  MAX_SAVING_MID_BENCH_MARK * MAX_SAVING_LOWER_INT_RATE + (amount - MAX_SAVING_MID_BENCH_MARK) * MAX_SAVING_MID_INT_RATE
        MAX_SAVING_MID_BENCH_MARK * MAX_SAVING_LOWER_INT_RATE + MAX_SAVING_HIGH_BENCH_MARK * MAX_SAVING_MID_INT_RATE + (amount - MAX_SAVING_HIGH_BENCH_MARK) * MAX_SAVING_UPPER_INT_RATE
      case _ =>
        amount * CHECKING_INT_RATE
    }
  }
}


class Account(val accountType: Account.AccountType.Value, val accoutHolder: String) {

  private val transactions = ListBuffer[Transaction]()

  private var balance: BigDecimal = BigDecimal(0.00)

  def deposit(amount: Option[BigDecimal]) = this.synchronized{
    if (amount.get > 0 ){
      balance = balance + amount.get
      transactions += Transaction(amount.get, Transaction.TransactionType.DEPOSIT)
    }else{
      throw new IllegalArgumentException("amount must be greater than 0")
    }
  }

  def withdraw(amount: Option[BigDecimal]) = this.synchronized{
    if (amount.get > 0 && amount.get <= balance){
      balance = balance - amount.get
      transactions += Transaction(amount.get, Transaction.TransactionType.WITHAW)
    }else if (amount.get > balance) {
      throw new IllegalArgumentException("amount must be less than account balance")
    }else{
      throw new IllegalArgumentException("amount must be greater than 0")
    }
  }

  def getBalance = this.balance

  def getTransactions = this.transactions

  override def toString: String = {
    val accountSummary = this.accountType + "" + "\n"
    val transactionsSummary  = transactions.foldLeft(""){(sum, transaction)=> sum + transaction.toString + "\n"}
    val total = "Total: " + balance
    accountSummary + transactionsSummary + total
  }

  def sumTransactions(checkAllTransactions: Boolean = true): BigDecimal = transactions.map(_.amount).sum

}