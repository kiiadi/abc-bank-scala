package com.abc

import org.scalatest.{FlatSpec, Matchers}

class TransactionTest extends FlatSpec with Matchers {
  "Transaction" should "type" in {
    val t = new Transaction(5)
    t.isInstanceOf[Transaction] should be(true)
  }
  
  "Transaction" should "have the amount of 5" in {
    val t = new Transaction(5)
    t.amount should be (5)
  }
}
