package org.bitcoins.core.bip47

sealed abstract class PaymentCodeVersion

object PaymentCodeVersion {
  case object V1 extends PaymentCodeVersion
  case object V2 extends PaymentCodeVersion
}
