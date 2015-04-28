package com.abc

import java.util.Calendar

case class Transaction(var amount: Double) {
  val date = Calendar.getInstance.getTime
}


