package com.abc

object AccountType extends Enumeration {
  type AccountType = Value
  val CHECKING = Value("Checking Account") 
  val SAVINGS  = Value("Savings Account") 
  val MAXI_SAVINGS = Value("Maxi-Savings Account")
}