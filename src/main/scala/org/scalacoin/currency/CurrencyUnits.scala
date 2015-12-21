package org.scalacoin.currency

import scala.math.BigDecimal.RoundingMode

abstract class BitcoinCurrencyUnit(val value:Double) {
  def toStringWithoutCurrencyLabel : String = CurrencyUnits.currencyFormatter(value)
  def ==(c : BitcoinCurrencyUnit) = {
    require (c != null, "Currency units cannot be null")
    CurrencyUnits.toSatoshis(this).value == CurrencyUnits.toSatoshis(c).value
  }
}

case class Satoshis(override val value: Double) extends BitcoinCurrencyUnit(value) {
  require(value.isValidInt, "The satoshis constructor cannot be a double with decimal places, satoshis are the smallest currency unit in bitcoin")
  override def toString = toStringWithoutCurrencyLabel + " Satoshis"
  override def toStringWithoutCurrencyLabel = value.toLong.toString
}

case class Bits(override val value: Double) extends BitcoinCurrencyUnit(value) {
  override def toString = toStringWithoutCurrencyLabel + " Bits"
}

case class Bitcoins(override val value: Double) extends BitcoinCurrencyUnit(value) {
  override def toString =  toStringWithoutCurrencyLabel + " BTC"
}

case class MilliBitcoins(override val value : Double) extends BitcoinCurrencyUnit(value) {
  override def toString = toStringWithoutCurrencyLabel + " mBTC"
}

object CurrencyUnits {
  val oneSatoshi = Satoshis(1)
  val oneMilliBit = Satoshis(100000)
  val tenMilliBits = Satoshis(1000000)
  val oneHundredMilliBits = Satoshis(10000000)



  /*considering the scalar for a 1 BTC to be 1*/
  val satoshiScalar = 0.00000001
  val bitsScalar = 0.000001
  val bitcoinScalar = 1
  val milliBitcoinScalar = 0.001
  def satoshisToBits(satoshis: Satoshis): Bits = {
    Bits((satoshis.value * satoshiScalar) / bitsScalar)
  }
  def sataoshisToBitcoin(satoshis: Satoshis): Bitcoins = {
    Bitcoins(satoshis.value * satoshiScalar)
  }

  def bitsToSatoshis(bits: Bits): Satoshis = {
    Satoshis((bits.value * (bitsScalar / satoshiScalar)).toLong)
  }

  def bitsToBitcoins(bits: Bits): Bitcoins = {
    Bitcoins((bits.value * bitsScalar) / bitcoinScalar)
  }

  def bitcoinsToSatoshis(bitcoins: Bitcoins): Satoshis = {

    Satoshis((bitcoins.value / satoshiScalar).toLong)
  }

  def bitcoinsToBits(bitcoins: Bitcoins): Bits = {
    Bits((bitcoinScalar * bitcoins.value) / bitsScalar)
  }

  def milliBitcoinsToSatoshis(milliBits : MilliBitcoins) = {
    Satoshis((milliBits.value * (milliBitcoinScalar / satoshiScalar)).toLong)
  }
  def currencyFormatter(value : Double) = {

    if (value == 0) "0" else "%.5f".format(BigDecimal(value).setScale(5, RoundingMode.CEILING)).replaceAll("[.0]*$","")
  }

  def toSatoshis(unit : BitcoinCurrencyUnit) = {
    unit match {
      case x : Bitcoins => bitcoinsToSatoshis(x)
      case x : Bits => bitsToSatoshis(x)
      case x : MilliBitcoins => milliBitcoinsToSatoshis(x)
      case x : Satoshis => x
    }
  }
}
