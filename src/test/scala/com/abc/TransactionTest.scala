package com.abc

import org.scalatest.{FlatSpec, Matchers}

class TransactionTest extends FlatSpec with Matchers {

  val MayTwentyThree2014 = 1400817600000L

  "a Transaction" should "propery construct" in {

    implicit val dateProvider = new DateProvider {
      override def now: Long = MayTwentyThree2014
    }

    val t = new Transaction(5)
    t should be(Transaction(5))

    // Assert that the internally generated transaction date is correct given the implicit DateProvider.
    t.transactionDate should be(MayTwentyThree2014)
  }
}
