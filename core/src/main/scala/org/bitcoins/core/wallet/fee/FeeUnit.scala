package org.bitcoins.core.wallet.fee

import org.bitcoins.core.currency.{CurrencyUnit, CurrencyUnits, Satoshis}
import org.bitcoins.core.protocol.transaction.Transaction

/**
  * This is meant to be an abstract type that represents different fee unit measurements for
  * blockchains
  */
sealed abstract class FeeUnit {
  def currencyUnit: CurrencyUnit
  def *(tx: Transaction): CurrencyUnit = calc(tx)
  def calc(tx: Transaction): CurrencyUnit = Satoshis(tx.vsize * toLong)
  def toLong: Long = currencyUnit.satoshis.toLong
}

/**
  * Meant to represent the different fee unit types for the bitcoin protocol
  * @see [[https://en.bitcoin.it/wiki/Weight_units]]
  */
sealed abstract class BitcoinFeeUnit extends FeeUnit

case class SatoshisPerByte(currencyUnit: CurrencyUnit) extends BitcoinFeeUnit {

  def toSatPerKb: SatoshisPerKiloByte = {
    SatoshisPerKiloByte(currencyUnit.satoshis * Satoshis(1000))
  }
}

object SatoshisPerByte {
  def fromLong(sats: Long): SatoshisPerByte = SatoshisPerByte(Satoshis(sats))
}

case class SatoshisPerKiloByte(currencyUnit: CurrencyUnit)
    extends BitcoinFeeUnit {

  def toSatPerByte: SatoshisPerByte = {
    val conversionOpt = (currencyUnit.toBigDecimal * 0.001).toBigIntExact
    conversionOpt match {
      case Some(conversion) =>
        val sat = Satoshis(conversion)
        SatoshisPerByte(sat)

      case None =>
        throw new RuntimeException(
          s"Failed to convert sat/kb -> sat/byte for ${currencyUnit}")
    }

  }
}

/**
  * A 'virtual byte' (also known as virtual size) is a new weight measurement that
  * was created with segregated witness (BIP141). Now 1 'virtual byte'
  * has the weight of 4 bytes in the [[org.bitcoins.core.protocol.transaction.TransactionWitness]]
  * of a [[org.bitcoins.core.protocol.transaction.WitnessTransaction]]
  */
case class SatoshisPerVirtualByte(currencyUnit: CurrencyUnit)
    extends BitcoinFeeUnit

object SatoshisPerVirtualByte {

  def fromLong(sats: Long): SatoshisPerVirtualByte =
    SatoshisPerVirtualByte(Satoshis(sats))

  val zero: SatoshisPerVirtualByte = SatoshisPerVirtualByte(CurrencyUnits.zero)
  val one: SatoshisPerVirtualByte = SatoshisPerVirtualByte(Satoshis.one)
}
