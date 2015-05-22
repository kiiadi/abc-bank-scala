package com.abc

case class Transaction(amount: Double)(implicit dateProvider: DateProvider) {
  val transactionDate = dateProvider.now
}

