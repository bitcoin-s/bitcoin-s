package org.bitcoins.core.protocol.ln


case class PaymentId(value: String) {
  override def toString: String = value
}

