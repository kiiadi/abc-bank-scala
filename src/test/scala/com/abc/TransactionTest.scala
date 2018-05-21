package com.abc

import org.scalatest.{FlatSpec, Matchers}

class TransactionTest extends FlatSpec with Matchers {
  "Deposit" should "type" in {
    val t = new Deposit(5)
    t.isInstanceOf[Transaction] should be(true)
    t.transactionType.toString should be("deposit")
  }
  "Withdraw" should "type" in {
    val t = new Withdraw(-5)
    t.isInstanceOf[Transaction] should be(true)
    t.transactionType.toString should be("withdraw")
  }

  "TransferFrom" should "type" in {
    val t = new TransferFrom(-5, "1", "2")
    t.isInstanceOf[Transaction] should be(true)
    t.transactionType.toString should be("transferFrom")
  }

  "TrasferTo" should "type" in {
    val t = new TransferTo(5, "1", "2")
    t.isInstanceOf[Transaction] should be(true)
    t.transactionType.toString should be("transferTo")
  }

}
