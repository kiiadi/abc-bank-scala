package com.abc

case class Transaction(var amount: Double) {
  val transactionDate = DateProvider.now
}

