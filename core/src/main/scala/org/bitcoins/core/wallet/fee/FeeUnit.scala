package org.bitcoins.core.wallet.fee

/**
  * This is meant to be an abstract type that represents different fee rates for
  * blockchains
  */
sealed abstract class FeeUnit

object FeeUnit {

  final case object PerByte extends FeeUnit

  final case object PerVirtualByte extends FeeUnit

  final case object PerKiloByte extends FeeUnit

  final case object Flat extends FeeUnit
}
