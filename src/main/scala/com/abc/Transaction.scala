package com.abc

import java.util.Calendar

import com.abc.Account.AccountType.Value

case class Transaction(val amount: BigDecimal,transactionType: Transaction.TransactionType.Value ) {
  val transactionDate =  Calendar.getInstance.getTime

  override def toString: String = {
    transactionType + " " +  amount
  }

}

object Transaction{
  object TransactionType extends Enumeration{
    type accountType = Value
    val DEPOSIT, WITHAW = Value
  }
}

