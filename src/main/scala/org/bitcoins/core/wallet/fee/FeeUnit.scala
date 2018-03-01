package org.bitcoins.core.wallet.fee

import org.bitcoins.core.currency.Satoshis

/** This is meant to be an abstract type that represents different fee unit measurements for
  * blockchains
  */
sealed abstract class FeeUnit

/** Meant to represent the different fee unit types for the bitcoin protocol
  * [[https://en.bitcoin.it/wiki/Weight_units]]
  */
sealed abstract class BitcoinFeeUnit

case class SatoshisPerByte(satoshis: Satoshis) extends FeeUnit

/** A 'virtual byte' (also known as virtual size) is a new weight measurement that
  * was created with segregated witness (BIP141). Now 1 'virtual byte'
  * has the weight of 4 bytes in the [[org.bitcoins.core.protocol.transaction.TransactionWitness]]
  * of a [[org.bitcoins.core.protocol.transaction.WitnessTransaction]]
  */
case class SatoshisPerVirtualByte(satoshis: Satoshis) extends FeeUnit
