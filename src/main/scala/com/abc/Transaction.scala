package com.abc

case class Transaction(var amount: Double) {
  val transactionDate = new DateProvider().now
}

