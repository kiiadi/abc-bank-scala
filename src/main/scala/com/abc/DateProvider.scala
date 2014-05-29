package com.abc

import java.util.Calendar
import java.util.Date

object DateProvider {

	private var instance: DateProvider = null

	private def getInstance: DateProvider = {
		if (instance == null) {
			instance = new DateProvider
		}
		instance
	}

	def apply(): DateProvider = {
		getInstance
	}
}

class DateProvider {
	protected def instance = Calendar.getInstance()

	def now: Date = {
		instance.getTime
	}

	def subtractDays(numDays: Int): Date = {
		val inst = instance
		inst.add(Calendar.DAY_OF_MONTH, -1 * numDays)
		inst.getTime
	}
}
