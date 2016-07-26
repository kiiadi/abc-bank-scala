package com.abc

import org.scalatest.{FlatSpec, Matchers}
import java.util.Date
import java.util.Calendar
import java.util.Formatter.DateTime

class MyTransactionTest extends FlatSpec with Matchers {
  "Transaction" should "type" in {
    val t = new MyTransaction(5)
    t.isInstanceOf[MyTransaction] should be(true)
  }
  
  it should "test amount" in {
    val t=new MyTransaction(110.10d);
    
    assert(t.amount == 110.10d)
  }
  
  it should "test primary constructor" in {
    val t=new MyTransaction(110.10d, new Date());
    
    assert(t.amount == 110.10d)
    //assert(t.transactionDate == new Date())
  }
  
  it should "test auxiliary constructor" in {
    val t=new MyTransaction(110.10d);
    
    assert(t.amount == 110.10d)
    //assert(t.transactionDate == new Date())
  }  
  
  it should "test toString" in {
    val format = new java.text.SimpleDateFormat("yyyy-MM-dd")  
    val d: Date= format.parse("2013-07-06")  
    val t=new MyTransaction(110.10d, d);
    t.toString() should be ("Transaction amount: $110.10, on date: 2013-07-06")
  }
}
