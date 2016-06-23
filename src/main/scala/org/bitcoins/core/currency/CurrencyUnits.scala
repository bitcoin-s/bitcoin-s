package org.bitcoins.core.currency

import org.bitcoins.core.number.{BaseNumbers, Int32, Int64, SignedNumber}
import org.bitcoins.core.protocol.NetworkElement
import org.bitcoins.core.serializers.{RawBitcoinSerializerHelper, RawSatoshisSerializer}
import org.bitcoins.core.util.{BitcoinSLogger, Factory}


sealed trait CurrencyUnit extends NetworkElement with BitcoinSLogger {
  type A
  def underlying : A

  def >=(c : CurrencyUnit) : Boolean = {
    CurrencyUnits.toSatoshis(this).underlying >= CurrencyUnits.toSatoshis(c).underlying
  }

  def >(c : CurrencyUnit) : Boolean = {
    CurrencyUnits.toSatoshis(this).underlying > CurrencyUnits.toSatoshis(c).underlying
  }

  def <(c : CurrencyUnit) : Boolean = {
    CurrencyUnits.toSatoshis(this).underlying < CurrencyUnits.toSatoshis(c).underlying
  }

  def <=(c : CurrencyUnit) : Boolean = {
    CurrencyUnits.toSatoshis(this).underlying <= CurrencyUnits.toSatoshis(c).underlying
  }

  def !=(c : CurrencyUnit) : Boolean = !(this == c)


  def +(c : CurrencyUnit) : CurrencyUnit = {
    Satoshis(CurrencyUnits.toSatoshis(this).underlying + CurrencyUnits.toSatoshis(c).underlying)
  }

  def -(c : CurrencyUnit) : CurrencyUnit = {
    Satoshis(CurrencyUnits.toSatoshis(this).underlying - CurrencyUnits.toSatoshis(c).underlying)
  }

  def *(c : CurrencyUnit) : CurrencyUnit = {
    Satoshis(CurrencyUnits.toSatoshis(this).underlying * CurrencyUnits.toSatoshis(c).underlying)
  }
}

sealed trait Satoshis extends CurrencyUnit {
  override type A = Int64
  override def hex = RawSatoshisSerializer.write(this)
}

object Satoshis extends Factory[Satoshis] with BaseNumbers[Satoshis] {
  private case class SatoshisImpl(underlying : Int64) extends Satoshis

  def min = Satoshis(Int64.min)
  def max = Satoshis(Int64.max)
  def zero = Satoshis(Int64.zero)
  def one = Satoshis(Int64.one)

  override def fromBytes(bytes : Seq[Byte]): Satoshis = RawSatoshisSerializer.read(bytes)

  def apply(int64: Int64): Satoshis = SatoshisImpl(int64)
}

object CurrencyUnits {
  def zero : CurrencyUnit = Satoshis.zero
  def negativeSatoshi = Satoshis(Int64(-1))

  def toSatoshis(unit : CurrencyUnit): Satoshis = unit match {
    case x : Satoshis => x
  }
}
