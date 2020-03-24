package org.bitcoins.core.wallet.fee

import org.bitcoins.core.consensus.Consensus
import org.bitcoins.core.currency.{CurrencyUnit, Satoshis}
import org.bitcoins.core.protocol.transaction.Transaction

/**
  * This is meant to be an abstract type that represents different fee rates for
  * blockchains
  */
sealed abstract class FeeRate {
  def *(tx: Transaction): CurrencyUnit = calc(tx)
  def calc(tx: Transaction): CurrencyUnit
  def units: FeeUnit
  def baseAmount: Double

}

/**
  * Meant to represent the different fee rate types for the bitcoin protocol
  * @see [[https://en.bitcoin.it/wiki/Weight_units]]
  */
sealed abstract class BitcoinFeeRate extends FeeRate {
  def sats: Double
  override def baseAmount: Double = sats

  def calc(tx: Transaction): CurrencyUnit = {
    val fee = tx.baseSize * baseAmount

    require(fee >= 0, "Fee cannot be negative")
    require(fee < Consensus.maxMoney.satoshis.toDouble,
            "Fee cannot be greater than the max value")

    Satoshis(fee.toLong)
  }
}

/** Represents how satoshis per byte of the transaction fee */
case class SatoshisPerByte(sats: Double) extends BitcoinFeeRate {

  def toSatPerKb: SatoshisPerKiloByte = {
    SatoshisPerKiloByte(sats * 1000)
  }

  override def units: FeeUnit = FeeUnit.PerByte
}

object SatoshisPerByte {

  def apply(sats: BigDecimal): SatoshisPerByte = SatoshisPerByte(sats.toDouble)

  val zero: SatoshisPerByte = SatoshisPerByte(0)
  val one: SatoshisPerByte = SatoshisPerByte(1)
}

/** Represents how satoshis per kilobyte of the transaction fee */
case class SatoshisPerKiloByte(sats: Double) extends BitcoinFeeRate {

  override def baseAmount: Double = sats * 0.001

  def toSatPerByte: SatoshisPerByte = {
    SatoshisPerByte(baseAmount)
  }

  override def units: FeeUnit = FeeUnit.PerKiloByte
}

object SatoshisPerKiloByte {

  def apply(sats: BigDecimal): SatoshisPerKiloByte =
    SatoshisPerKiloByte(sats.toDouble)

  val zero: SatoshisPerKiloByte = SatoshisPerKiloByte(0)
  val one: SatoshisPerKiloByte = SatoshisPerKiloByte(1)
}

/**
  * A 'virtual byte' (also known as virtual size) is a new weight measurement that
  * was created with segregated witness (BIP141). Now 1 'virtual byte'
  * has the weight of 4 bytes in the [[org.bitcoins.core.protocol.transaction.TransactionWitness]]
  * of a [[org.bitcoins.core.protocol.transaction.WitnessTransaction]]
  */
case class SatoshisPerVirtualByte(sats: Double) extends BitcoinFeeRate {
  override def calc(tx: Transaction): CurrencyUnit = {
    val fee = tx.vsize * baseAmount

    require(fee < Consensus.maxMoney.satoshis.toDouble,
            "Fee cannot be greater than the max value")

    Satoshis(fee.toLong)
  }

  override def units: FeeUnit = FeeUnit.PerVirtualByte
}

object SatoshisPerVirtualByte {

  def apply(sats: BigDecimal): SatoshisPerVirtualByte =
    SatoshisPerVirtualByte(sats.toDouble)

  val zero: SatoshisPerVirtualByte = SatoshisPerVirtualByte(0)
  val one: SatoshisPerVirtualByte = SatoshisPerVirtualByte(1)
}

/** Represents how many satoshis to be paid for the transaction fee */
case class FlatSatoshis(satoshis: Long) extends BitcoinFeeRate {
  override def sats: Double = satoshis.toDouble

  require(satoshis <= Consensus.maxMoney.satoshis.toDouble,
          "Fee cannot be greater than the max value")

  def currencyUnit: Satoshis = Satoshis(satoshis)

  override def calc(tx: Transaction): CurrencyUnit = {
    Satoshis(satoshis)
  }

  override def units: FeeUnit = FeeUnit.Flat
}
