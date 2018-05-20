package com.abc

import java.util.Date

sealed trait Transaction {
  val amount: Double = 0
  val transactionDate: Date = DateUtils.now
  def transactionType: String
}

case class Withdrawl(override val amount: Double) extends Transaction  {
  override def transactionType = "withdrawal"
}
case class Deposit(override val amount: Double) extends Transaction  {
  override def transactionType = "deposit"
}
case class TransferFrom(override val amount: Double, fromAccountId: String, toAccountId: String) extends Transaction  {
  override def transactionType = "transferFrom"
}
case class TransferTo(override val amount: Double, fromAccountId: String, toAccountId: String) extends Transaction  {
  override def transactionType = "transferTo"
}



