package com.abc

import java.util.Date

object TransactionType extends Enumeration{
  type transactionType = Value
  val withdraw, deposit, transferFrom, transferTo = Value
}
sealed trait Transaction {
  val amount: Double = 0
  val transactionDate: Date = DateUtils.now
  def transactionType: TransactionType.transactionType
}

case class Withdraw(override val amount: Double, override val transactionDate: Date = DateUtils.now) extends Transaction  {
  override def transactionType = TransactionType.withdraw
}
case class Deposit(override val amount: Double, override val transactionDate: Date = DateUtils.now) extends Transaction  {
  override def transactionType = TransactionType.deposit
}
case class TransferFrom(override val amount: Double, fromAccountId: String, toAccountId: String,
                        override val transactionDate: Date = DateUtils.now) extends Transaction  {
  override def transactionType = TransactionType.transferFrom
}
case class TransferTo(override val amount: Double, fromAccountId: String, toAccountId: String,
                      override val transactionDate: Date = DateUtils.now) extends Transaction  {
  override def transactionType = TransactionType.transferTo
}



