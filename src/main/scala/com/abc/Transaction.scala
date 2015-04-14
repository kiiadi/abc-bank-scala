package com.abc

import java.util.Date

 class Transaction(val amount: Double) {
  val transactionDate = DateProvider.instance.now
  
  private  def withdrawalOrDepositText =
    this.amount match {
      case a if a < 0 => "withdrawal"
      case a if a > 0 => "deposit"
      case _ => "N/A"
    }
  private def toDollars : String = f"$$$amount%.2f"
  override val toString = withdrawalOrDepositText + " " + toDollars
  
}

// the following is only for testing
case class TestTransaction( override val amount: Double,override val transactionDate : Date) extends Transaction (amount: Double);