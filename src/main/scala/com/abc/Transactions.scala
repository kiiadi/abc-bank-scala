package com.abc

import com.abc.Formatting._

import scala.collection.mutable.ListBuffer

class Transactions {
  private val transactions: ListBuffer[Transaction] = ListBuffer()

  def +=(transaction: Transaction) =
    transactions += transaction

  def sum: Double =
    transactions.map(_.amount).sum

  def getSummary: String = {
    val transactionSummary = transactions.map(_.getSummary)
      .mkString("  ", "\n  ", "\n")
    val totalSummary = s"Total ${toDollars(transactions.map(_.amount).sum)}"
    transactionSummary + totalSummary
  }
}
