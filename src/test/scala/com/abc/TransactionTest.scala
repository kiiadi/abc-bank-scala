package com.abc

import org.scalatest.{FlatSpec, Matchers}

class TransactionTest extends FlatSpec with Matchers {
	"Transaction" should "have a valid type" in {
		val t = new Transaction(5, DateProvider().now)
		t.isInstanceOf[Transaction] should be(true)
	}
}