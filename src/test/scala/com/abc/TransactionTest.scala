package com.abc

import org.scalatest.{FlatSpec, Matchers}

class TransactionTest extends FlatSpec with Matchers {
  implicit val dateProvider = DateProvider.getInstance

  "Transaction" should "type" in {
    val t = Transaction(5)
    t.isInstanceOf[Transaction] should be(true)
  }
}
