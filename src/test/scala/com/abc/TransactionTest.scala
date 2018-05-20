package com.abc

import org.scalatest.{FlatSpec, Matchers}

class TransactionTest extends FlatSpec with Matchers {
  "Deposit" should "type" in {
    val t = new Deposit(5)
    t.isInstanceOf[Transaction] should be(true)
  }
  "Withdrawl" should "type" in {
    val t = new Withdrawl(-5)
    t.isInstanceOf[Transaction] should be(true)
  }

  "TransferFrom" should "type" in {
    val t = new TransferFrom(-5, "1", "2")
    t.isInstanceOf[Transaction] should be(true)
  }

  "TrasferTo" should "type" in {
    val t = new TransferTo(-5, "1", "2")
    t.isInstanceOf[Transaction] should be(true)
  }

}
