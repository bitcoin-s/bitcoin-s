package org.bitcoins.core.wallet.fee

import org.bitcoins.core.currency._
import org.bitcoins.core.protocol.transaction.Transaction

/**
  * This is meant to be an abstract type that represents different fee unit measurements for
  * blockchains
  */
sealed abstract class FeeUnit {
  def currencyUnit: CurrencyUnit
  final def *(cu: CurrencyUnit): CurrencyUnit = this * cu.satoshis.toLong
  final def *(int: Int): CurrencyUnit = this * int.toLong

  final def *(long: Long): CurrencyUnit = Satoshis(toLong * long / scaleFactor)
  def *(tx: Transaction): CurrencyUnit = calc(tx)

  /** The coefficient the denominator in the unit is multiplied by,
    * for example sats/kilobyte -> 1000
    */
  def scaleFactor: Long

  require(scaleFactor > 0,
          s"Scale factor cannot be less than or equal to 0, got $scaleFactor")

  /** Takes the given transaction returns a size that will be used for calculating the fee rate.
    * This is generally the denominator in the unit, ie sats/byte
    */
  def txSizeForCalc(tx: Transaction): Long

  /** Calculates the fee for the transaction using this fee rate, rounds down satoshis */
  final def calc(tx: Transaction): CurrencyUnit = this * txSizeForCalc(tx)
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

  override def txSizeForCalc(tx: Transaction): Long = tx.byteSize

  override def scaleFactor: Long = 1
}

object SatoshisPerByte {
  def fromLong(sats: Long): SatoshisPerByte = SatoshisPerByte(Satoshis(sats))
}

/**
  * KiloBytes here are defined as 1000 bytes.
  */
case class SatoshisPerKiloByte(currencyUnit: CurrencyUnit)
    extends BitcoinFeeUnit {

  lazy val toSatPerByteExact: SatoshisPerByte = {
    val conversionOpt = (currencyUnit.toBigDecimal / 1000.0).toBigIntExact
    conversionOpt match {
      case Some(conversion) =>
        val sat = Satoshis(conversion)
        SatoshisPerByte(sat)

      case None =>
        throw new RuntimeException(
          s"Failed to convert sat/kb -> sat/byte (loss of precision) for $currencyUnit")
    }
  }

  lazy val toSatPerByteRounded: SatoshisPerByte = {
    val conversion = (currencyUnit.toBigDecimal / 1000.0).toBigInt
    SatoshisPerByte(Satoshis(conversion))
  }

  lazy val toSatPerByte: SatoshisPerByte = toSatPerByteExact

  override def txSizeForCalc(tx: Transaction): Long = tx.byteSize

  override def scaleFactor: Long = 1000
}

/**
  * A 'virtual byte' (also known as virtual size) is a new weight measurement that
  * was created with segregated witness (BIP141). Now 1 'virtual byte'
  * has the weight of 4 bytes in the [[org.bitcoins.core.protocol.transaction.TransactionWitness]]
  * of a [[org.bitcoins.core.protocol.transaction.WitnessTransaction]]
  */
case class SatoshisPerVirtualByte(currencyUnit: CurrencyUnit)
    extends BitcoinFeeUnit {
  override def txSizeForCalc(tx: Transaction): Long = tx.vsize

  override def scaleFactor: Long = 1
}

object SatoshisPerVirtualByte {

  val zero: SatoshisPerVirtualByte = SatoshisPerVirtualByte(CurrencyUnits.zero)
  val one: SatoshisPerVirtualByte = SatoshisPerVirtualByte(Satoshis.one)
}

/**
  * Weight is used to indicate how 'expensive' the transaction is on the blockchain.
  * This use to be a simple calculation before segwit (BIP141). Each byte in the transaction
  * counted as 4 'weight' units. Now with segwit, the
  * [[org.bitcoins.core.protocol.transaction.TransactionWitness TransactionWitness]]
  * is counted as 1 weight unit per byte,
  * while other parts of the transaction (outputs, inputs, locktime etc) count as 4 weight units.
  * As we add more witness versions, this may be subject to change.
  * [[https://github.com/bitcoin/bips/blob/master/bip-0141.mediawiki#Transaction_size_calculations BIP 141]]
  * [[https://github.com/bitcoin/bitcoin/blob/5961b23898ee7c0af2626c46d5d70e80136578d3/src/consensus/validation.h#L96]]
  */
case class SatoshisPerKW(currencyUnit: CurrencyUnit) extends BitcoinFeeUnit {

  override def txSizeForCalc(tx: Transaction): Long = tx.weight

  override def scaleFactor: Long = 1000
}

object SatoshisPerKW {

  val zero: SatoshisPerKW = SatoshisPerKW(CurrencyUnits.zero)
  val one: SatoshisPerKW = SatoshisPerKW(Satoshis.one)
}
