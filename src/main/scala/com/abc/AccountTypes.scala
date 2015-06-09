package com.abc

object AccountTypes {
  sealed trait AccountType
  case object Checking extends AccountType
  case object Savings extends AccountType
  case object MaxiSavings extends AccountType
}
