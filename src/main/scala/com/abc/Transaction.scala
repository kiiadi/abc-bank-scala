package com.abc

case class Transaction(val amount: Double, val transactionDate : java.util.Date = DateProvider.now)
