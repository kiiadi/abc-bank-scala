package com.abc

import com.abc.Formatting._

case class Transaction(amount: Double) {
  def getSummary : String =
    s"$withdrawalOrDepositText ${toDollars(amount.abs)}"

  private def withdrawalOrDepositText = amount match {
    case a if a < 0 => "withdrawal"
    case a if a > 0 => "deposit"
    case _ => "N/A"
  }
}

