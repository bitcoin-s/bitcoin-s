package org.bitcoins.core.wallet.fee

import org.bitcoins.core.currency.{CurrencyUnit, Satoshis}
import org.bitcoins.core.protocol.transaction.Transaction

/**
  * This is meant to be an abstract type that represents different fee unit measurements for
  * blockchains
  */
sealed abstract class FeeUnit {
  def *(tx: Transaction): CurrencyUnit = calc(tx)
  def calc(tx: Transaction): CurrencyUnit
  def baseAmount: Double
}

/**
  * Meant to represent the different fee unit types for the bitcoin protocol
  * @see [[https://en.bitcoin.it/wiki/Weight_units]]
  */
sealed abstract class BitcoinFeeUnit extends FeeUnit {
  def sats: Double
  override def baseAmount: Double = sats

  def calc(tx: Transaction): CurrencyUnit =
    Satoshis((tx.baseSize * baseAmount).toLong)
}

case class SatoshisPerByte(sats: Double) extends BitcoinFeeUnit {

  def toSatPerKb: SatoshisPerKiloByte = {
    SatoshisPerKiloByte(sats * 1000)
  }
}

case class SatoshisPerKiloByte(sats: Double) extends BitcoinFeeUnit {

  override def baseAmount: Double = sats * 0.001

  def toSatPerByte: SatoshisPerByte = {
    SatoshisPerByte(baseAmount)
  }
}

/**
  * A 'virtual byte' (also known as virtual size) is a new weight measurement that
  * was created with segregated witness (BIP141). Now 1 'virtual byte'
  * has the weight of 4 bytes in the [[org.bitcoins.core.protocol.transaction.TransactionWitness]]
  * of a [[org.bitcoins.core.protocol.transaction.WitnessTransaction]]
  */
case class SatoshisPerVirtualByte(sats: Double) extends BitcoinFeeUnit {
  override def calc(tx: Transaction): CurrencyUnit =
    Satoshis((tx.vsize * baseAmount).toLong)
}

object SatoshisPerVirtualByte {
  val zero: SatoshisPerVirtualByte = SatoshisPerVirtualByte(0)
  val one: SatoshisPerVirtualByte = SatoshisPerVirtualByte(1)
}
