package com.abc

import org.scalatest.{FlatSpec, Matchers}

class TransactionTest extends FlatSpec with Matchers {
  
  "Transaction" should "be of correct type" in {
    val t = new Transaction(5)
    t.isInstanceOf[Transaction] should be(true)
  }
  
  it  should "have the amount of 5" in {
    val t = new Transaction(5)
    t.amount should be (5)
  }
  
  it  should "print out toString as expected" in {
    val t = new Transaction(5)
    t.toString should be ("deposit $5.00")
  }
}
