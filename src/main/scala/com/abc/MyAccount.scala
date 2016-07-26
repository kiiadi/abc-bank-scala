package com.abc

import scala.collection.mutable.ListBuffer
import com.abc.AccountType._

import java.util.Date

class MyAccount(val accountType: AccountType, var transactions: ListBuffer[MyTransaction] = ListBuffer[MyTransaction]()) {
  var balance: Double = 0.0d

  def deposit(amount: Double) {
      require(amount>0)
      transactions += new MyTransaction(amount)
      balance+=amount;
  }
  
  def deposit(amount: Double, transactionDate: Date= new Date): Unit ={
      require(amount>0)
      transactions += new MyTransaction(amount,transactionDate)
      balance+=amount;
  }  

  def withdraw(amount: Double) {
      require(amount>0)
      transactions += new MyTransaction(-amount)
      balance-=amount;
  }
  
  def withdraw(amount: Double, transactionDate: Date= new Date) : Unit= {
      require(amount>0)
      transactions += new MyTransaction(-amount, transactionDate)
      balance-=amount;
  }  

  def interestEarned: Double = {
    //val amount: Double = sumTransactions()
    accountType match {
      case SAVINGS =>
          savingsAccountInterest
      case MAXI_SAVINGS =>
          maxiSavingsAccountInterest
      case CHECKING =>
          checkingAccountInterest
    }
  }
  
  def checkingAccountInterest: Double = {
    var totalCheckingInterest=0d;
    val tArray: Array[MyTransaction]=transactions.sortBy(_.transactionDate).toArray
    var balance : Double =0.0d;  
    if(tArray.size>=2){
      for(i<- 1 until tArray.size-1){
         balance+=tArray(i-1).amount
         val days : Long = (tArray(i).transactionDate.getTime-tArray(i-1).transactionDate.getTime)/(24 * 60 * 60 * 1000);
            totalCheckingInterest += balance * 0.001*(days/365d);
      }
    }
    
    //last transaction to today/report date interest
    if(tArray.size>=1){
       balance+=tArray(tArray.size-1).amount
       val d : Long = ((new Date).getTime-tArray(tArray.size-1).transactionDate.getTime)/(24 * 60 * 60 * 1000);
       totalCheckingInterest += balance * 0.001*(d/365d); 
    }
    
    totalCheckingInterest    
  }
  
  def savingsAccountInterest: Double = {
    var totalSavingsInterest=0d;
    val tArray: Array[MyTransaction]=transactions.sortBy(_.transactionDate).toArray
    var balance : Double =0.0d;  
    if(tArray.size>=2){
      for(i<- 1 until tArray.size-1){
         balance+=tArray(i-1).amount
         val d : Long = (tArray(i).transactionDate.getTime-tArray(i-1).transactionDate.getTime)/(24 * 60 * 60 * 1000);
         if (balance <= 1000) {
            totalSavingsInterest += balance * 0.001*(d/365d);
         }else {
            totalSavingsInterest += 1*(d/365d) + (balance - 1000) * 0.002*(d/365d)      
         }
      }
    }
    
    //last transaction to today/report date interest
    if(tArray.size>=1){
     balance+=tArray(tArray.size-1).amount
     val d : Long = ((new Date).getTime-tArray(tArray.size-1).transactionDate.getTime)/(24 * 60 * 60 * 1000);
     if (balance <= 1000) {
        totalSavingsInterest += balance * 0.001*(d/365d);
     }else {
        totalSavingsInterest += 1*(d/365d) + (balance - 1000) * 0.002*(d/365d)       
     } 
    }
    
    totalSavingsInterest
  }
  
  def maxiSavingsAccountInterest: Double = {
    var totalSavingsInterest=0d;
    val tArray: Array[MyTransaction]=transactions.sortBy(_.transactionDate).toArray
    var balance : Double =0.0d;  
    if(tArray.size>=2){
      for(i<- 1 until tArray.size-1){
         balance+=tArray(i-1).amount
         val d : Long = (tArray(i).transactionDate.getTime-tArray(i-1).transactionDate.getTime)/(24 * 60 * 60 * 1000);
         if (balance <= 1000) {
            totalSavingsInterest += balance * 0.02*(d/365d);
         }else if(balance>1000 && balance<=2000){
            totalSavingsInterest += 20*(d/365d) + (balance - 1000) * 0.05*(d/365d)       
         }else{
           totalSavingsInterest += 70*(d/365d) + (balance - 2000) * 0.1*(d/365d)
         }
      }
    }
    
    //last transaction to today/report date interest
    if(tArray.size>=1){
       balance+=tArray(tArray.size-1).amount
       val d : Long = ((new Date).getTime-tArray(tArray.size-1).transactionDate.getTime)/(24 * 60 * 60 * 1000);
       if (balance <= 1000) {
          totalSavingsInterest += balance * 0.02*(d/365d);
       }else if(balance>1000 && balance<=2000){
          totalSavingsInterest += 20*(d/365d) + (balance - 1000) * 0.05*(d/365d)         
       }else{
         totalSavingsInterest += 70*(d/365d) + (balance - 2000) * 0.1*(d/365d)
       } 
    }
    
    totalSavingsInterest
  }  

  //def sumTransactions(checkAllTransactions: Boolean = true): Double = transactions.map(_.amount).sum
  def sumTransactions(): Double = balance

}