package com.abc

import org.scalatest.{FlatSpec, Matchers}

class TransactionTest extends FlatSpec with Matchers {
  "Transaction" when {
    val t = new Transaction(5)
    t.isInstanceOf[Transaction] should be(true)
  }
}
