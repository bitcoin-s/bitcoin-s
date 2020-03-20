package org.bitcoins.core.wallet.fee

/**
  * This is meant to be an abstract type that represents different fee rates for
  * blockchains
  */
sealed abstract class FeeUnit

object FeeUnit {

  /** Represents how many units of currency per byte of the transaction fee */
  final case object PerByte extends FeeUnit

  /** Represents how many units of currency per virtual byte of the transaction fee */
  final case object PerVirtualByte extends FeeUnit

  /** Represents how many units of currency per kilobyte of the transaction fee */
  final case object PerKiloByte extends FeeUnit

  /** Represents how many units of currency to be paid for the transaction fee */
  final case object Flat extends FeeUnit
}
