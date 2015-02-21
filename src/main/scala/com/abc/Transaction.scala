package com.abc

case class Transaction(var amount: Double) {
  val transactionDate = DateProvider.now

  // DateProvider.now.getTime() returns time in milliseconds after a specific time.
  // Convert milliseconds time to number of days
  def elapsedDays: Long = (DateProvider.now.getTime() - transactionDate.getTime()) / (24 * 3600 * 100 )
}

