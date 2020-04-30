package org.bitcoins.core.currency

import org.bitcoins.core.consensus.Consensus
import org.bitcoins.core.number.{BaseNumbers, BasicArithmetic, Bounded, Int64}
import org.bitcoins.core.serializers.RawSatoshisSerializer
import org.bitcoins.crypto.{Factory, NetworkElement}
import scodec.bits.ByteVector

import scala.math.Numeric
import scala.util.{Failure, Success, Try}

sealed abstract class CurrencyUnit
    extends NetworkElement
    with BasicArithmetic[CurrencyUnit] {
  type A

  def satoshis: Satoshis

  // Cannot use the override modifier because this method was added in scala version 2.13
  def parseString(str: String): Option[CurrencyUnit] = {
    if (str.isEmpty) {
      None
    } else {
      Try(str.toLong) match {
        case Success(num) => Some(Satoshis(num))
        case Failure(_)   => None
      }
    }
  }

  def compare(c: CurrencyUnit): Int =
    CurrencyUnits.compare(this, c)

  def !=(c: CurrencyUnit): Boolean = !(this == c)

  def ==(c: CurrencyUnit): Boolean = satoshis == c.satoshis

  def <(c: CurrencyUnit): Boolean = satoshis.underlying < c.satoshis.underlying

  def <=(c: CurrencyUnit): Boolean =
    satoshis.underlying <= c.satoshis.underlying

  def >(c: CurrencyUnit): Boolean = satoshis.underlying > c.satoshis.underlying

  def >=(c: CurrencyUnit): Boolean =
    satoshis.underlying >= c.satoshis.underlying

  override def +(c: CurrencyUnit): CurrencyUnit = {
    Satoshis(satoshis.underlying + c.satoshis.underlying)
  }

  override def -(c: CurrencyUnit): CurrencyUnit = {
    Satoshis(satoshis.underlying - c.satoshis.underlying)
  }

  override def *(factor: BigInt): CurrencyUnit = {
    Satoshis(satoshis.underlying * factor)
  }

  override def *(c: CurrencyUnit): CurrencyUnit = {
    Satoshis(satoshis.underlying * c.satoshis.underlying)
  }

  def unary_- : CurrencyUnit = {
    Satoshis(-satoshis.underlying)
  }

  override def bytes: ByteVector = satoshis.bytes

  def toBigDecimal: BigDecimal

  protected def underlying: A
}

sealed abstract class Satoshis extends CurrencyUnit {
  override type A = Int64

  override def toString: String = {
    val num = toLong
    val postFix = if (num == 1) "sat" else "sats"
    s"$num $postFix"
  }

  override def bytes: ByteVector = RawSatoshisSerializer.write(this)

  override def satoshis: Satoshis = this

  override def toBigDecimal = BigDecimal(toBigInt)

  def toBigInt: BigInt = BigInt(toLong)

  def toLong: Long = underlying.toLong

  def ==(satoshis: Satoshis): Boolean = underlying == satoshis.underlying
}

object Satoshis
    extends Factory[Satoshis]
    with BaseNumbers[Satoshis]
    with Bounded[Satoshis] {

  val min = Satoshis(Int64.min)
  val max = Satoshis(Int64.max)
  val zero = Satoshis(Int64.zero)
  val one = Satoshis(Int64.one)

  override def fromBytes(bytes: ByteVector): Satoshis =
    RawSatoshisSerializer.read(bytes)
  def apply(int64: Int64): Satoshis = SatoshisImpl(int64)
  def apply(satoshis: Long): Satoshis = SatoshisImpl(Int64(satoshis))
  def apply(satoshis: BigInt): Satoshis = SatoshisImpl(Int64(satoshis))

  private case class SatoshisImpl(underlying: Int64) extends Satoshis
}

sealed abstract class Bitcoins extends CurrencyUnit {
  override type A = BigDecimal

  override def toString: String = s"$toBigDecimal BTC"

  override def toBigDecimal: BigDecimal = underlying

  override def hex: String = satoshis.hex

  override def satoshis: Satoshis = {
    val sat = underlying * CurrencyUnits.btcToSatoshiScalar
    Satoshis(sat.toLongExact)
  }
}

object Bitcoins extends BaseNumbers[Bitcoins] with Bounded[Bitcoins] {
  val min = Bitcoins((-Consensus.maxMoney).satoshis)
  val max = Bitcoins(Consensus.maxMoney.satoshis)
  val zero = Bitcoins(Satoshis.zero)
  val one = Bitcoins(1)

  def apply(satoshis: Satoshis): Bitcoins = {
    val b: BigDecimal = satoshis.toLong * CurrencyUnits.satoshisToBTCScalar
    Bitcoins(b)
  }

  def apply(underlying: BigDecimal): Bitcoins = BitcoinsImpl(underlying)

  private case class BitcoinsImpl(underlying: BigDecimal) extends Bitcoins
}

object CurrencyUnits extends Numeric[CurrencyUnit] {

  override def plus(x: CurrencyUnit, y: CurrencyUnit): CurrencyUnit = x + y

  override def minus(x: CurrencyUnit, y: CurrencyUnit): CurrencyUnit = x - y

  override def times(x: CurrencyUnit, y: CurrencyUnit): CurrencyUnit = x * y

  override def negate(x: CurrencyUnit): CurrencyUnit = -x

  override def fromInt(x: Int): CurrencyUnit = Satoshis(x.toLong)

  // Must go through toLong to avoid infinite recursion
  override def toInt(x: CurrencyUnit): Int = x.satoshis.toLong.toInt

  override def toLong(x: CurrencyUnit): Long = x.satoshis.toLong

  override def toFloat(x: CurrencyUnit): Float =
    x.satoshis.toBigInt.toFloat

  override def toDouble(x: CurrencyUnit): Double =
    x.satoshis.toBigInt.toDouble

  override def compare(x: CurrencyUnit, y: CurrencyUnit): Int =
    x.satoshis.toBigInt compare y.satoshis.toBigInt

  // Cannot use the override modifier because this method was added in scala version 2.13
  def parseString(str: String): Option[CurrencyUnit] = {
    if (str.isEmpty) {
      None
    } else {
      Try(str.toLong) match {
        case Success(num) => Some(Satoshis(num))
        case Failure(_)   => None
      }
    }
  }

  /** The number you need to multiply BTC by to get it's satoshis */
  val btcToSatoshiScalar: Long = 100000000
  val satoshisToBTCScalar: BigDecimal = BigDecimal(1.0) / btcToSatoshiScalar
  val oneBTC: CurrencyUnit = Satoshis(btcToSatoshiScalar)
  val oneMBTC: CurrencyUnit = Satoshis(btcToSatoshiScalar / 1000)
  override val zero: CurrencyUnit = Satoshis.zero
  val negativeSatoshi = Satoshis(-1)

  def toSatoshis(unit: CurrencyUnit): Satoshis = unit match {
    case b: Bitcoins => b.satoshis
    case x: Satoshis => x
  }
}
