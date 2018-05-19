package com.abc

import java.util.Date

case class Transaction(val amount: Double, val transactionDate: Date = DateUtils.now ) {
   def withdrawalOrDepositText: String =
    amount match {
      case a if a < 0 => "withdrawal"
      case a if a > 0 => "deposit"
      case _ => "N/A"
    }

}

