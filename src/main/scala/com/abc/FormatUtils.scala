package com.abc

object FormatUtils {
  def toDollars(number: Double): String = f"$$$number%.2f"

  def formatPlural(number: Int, word: String): String = {
    number + " " + (if (number == 1) word else word + "s")
  }

}

