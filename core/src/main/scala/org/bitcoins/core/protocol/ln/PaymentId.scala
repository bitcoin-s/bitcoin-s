package org.bitcoins.core.protocol.ln

import java.util.UUID

case class PaymentId(value: String) {
  override def toString: String = value

  def toUUID: UUID = UUID.fromString(value)
}

