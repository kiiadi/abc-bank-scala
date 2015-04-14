package com.abc

import java.util.Date

 class Transaction(val amount: Double) {
  val transactionDate = DateProvider.instance.now
}


case class TestTransaction( override val amount: Double,override val transactionDate : Date) extends Transaction (amount: Double);