package com.abc

/**
 * A transaction on an Account, which includes a transaction date.
 * @param amount Double the amount of the transaction.
 * @param dateProvider DateProvider the implicit date provider.
 */
case class Transaction(amount: Double)(implicit dateProvider: DateProvider) {
  val transactionDate: Long = dateProvider.now
}

