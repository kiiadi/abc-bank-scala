package com.abc

import java.util.{Calendar, Date}

object Condition {
  def check(cond: Boolean, ex: Throwable) : Unit = {
    if (cond) throw ex
  }
}

