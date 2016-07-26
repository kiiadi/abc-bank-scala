package com.abc


object AccountType {  
  sealed trait AccountType 
  case object CHECKING extends AccountType
  case object SAVINGS extends AccountType
  case object MAXI_SAVINGS extends AccountType
}