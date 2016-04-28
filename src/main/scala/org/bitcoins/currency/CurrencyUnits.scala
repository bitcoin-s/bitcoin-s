package org.bitcoins.currency

import scala.math.BigDecimal.RoundingMode

abstract class CurrencyUnit(val value:Double) {
  def toStringWithoutCurrencyLabel : String = CurrencyUnits.currencyFormatter(value)
  def ==(c : CurrencyUnit) : Boolean = {
    /*require (c != null, "Currency units cannot be null")*/
    CurrencyUnits.toSatoshis(this).value == CurrencyUnits.toSatoshis(c).value
  }

  def >=(c : CurrencyUnit) : Boolean = {
    CurrencyUnits.toSatoshis(this).value >= CurrencyUnits.toSatoshis(c).value
  }

  def >(c : CurrencyUnit) : Boolean = {
    CurrencyUnits.toSatoshis(this).value > CurrencyUnits.toSatoshis(c).value
  }

  def <(c : CurrencyUnit) : Boolean = {
    CurrencyUnits.toSatoshis(this).value < CurrencyUnits.toSatoshis(c).value
  }

  def <=(c : CurrencyUnit) : Boolean = {
    /*require (c != null, "Currency units cannot be null")*/
    CurrencyUnits.toSatoshis(this).value <= CurrencyUnits.toSatoshis(c).value
  }

  def !=(c : CurrencyUnit) : Boolean = {
    !(this == c)
  }

  def +(c : CurrencyUnit) : CurrencyUnit = {
    Satoshis(CurrencyUnits.toSatoshis(this).value + CurrencyUnits.toSatoshis(c).value)
  }

  def -(c : CurrencyUnit) : CurrencyUnit = {
    Satoshis(CurrencyUnits.toSatoshis(this).value - CurrencyUnits.toSatoshis(c).value)
  }

  def *(c : CurrencyUnit) : CurrencyUnit = {
    Satoshis(CurrencyUnits.toSatoshis(this).value * CurrencyUnits.toSatoshis(c).value)
  }
}

case class Satoshis(override val value: Double) extends CurrencyUnit(value) {
  require(value.isWhole, "The satoshis constructor cannot be a double with decimal places, satoshis are the smallest currency unit in bitcoin")
  override def toString = toStringWithoutCurrencyLabel + " Satoshis"
  override def toStringWithoutCurrencyLabel = value.toLong.toString
}

case class Bits(override val value: Double) extends CurrencyUnit(value) {
  override def toString = toStringWithoutCurrencyLabel + " Bits"
}

case class Bitcoins(override val value: Double) extends CurrencyUnit(value) {
  override def toString =  toStringWithoutCurrencyLabel + " BTC"
}

case class MilliBitcoins(override val value : Double) extends CurrencyUnit(value) {
  override def toString = toStringWithoutCurrencyLabel + " mBTC"
}

object CurrencyUnits {
  def negativeSatoshi = Satoshis(-1)
  def zeroSatoshis = Satoshis(0)
  def oneSatoshi = Satoshis(1)
  def oneMilliBit = Satoshis(100000)
  def tenMilliBits = Satoshis(1000000)
  def oneHundredMilliBits = Satoshis(10000000)

  def oneBTC = Bitcoins(1)

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

  def toSatoshis(unit : CurrencyUnit) = {
    unit match {
      case x : Bitcoins => bitcoinsToSatoshis(x)
      case x : Bits => bitsToSatoshis(x)
      case x : MilliBitcoins => milliBitcoinsToSatoshis(x)
      case x : Satoshis => x
    }
  }
}
