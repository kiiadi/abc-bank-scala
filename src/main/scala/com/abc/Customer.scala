package com.abc

case class Customer(val name: String) {

	override def equals(obj:Any) = {
		obj.isInstanceOf[Customer] && obj.asInstanceOf[Customer].name == this.name
	}

	override def toString() = {
		name
	}
}
