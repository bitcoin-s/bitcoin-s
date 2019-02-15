package org.bitcoins.core.protocol.ln.currency

import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.number.{BaseNumbers, BasicArithmetic, Int64, UInt5}
import org.bitcoins.core.protocol.NetworkElement
import org.bitcoins.core.protocol.ln._
import org.bitcoins.core.util.Bech32
import scodec.bits.ByteVector

import scala.util.{Failure, Try}

sealed abstract class LnCurrencyUnit
    extends NetworkElement
    with BasicArithmetic[LnCurrencyUnit] {
  def character: Char

  def >=(ln: LnCurrencyUnit): Boolean = {
    toPicoBitcoinValue >= ln.toPicoBitcoinValue
  }

  def >(ln: LnCurrencyUnit): Boolean = {
    toPicoBitcoinValue > ln.toPicoBitcoinValue
  }

  def <(ln: LnCurrencyUnit): Boolean = {
    toPicoBitcoinValue < ln.toPicoBitcoinValue
  }

  def <=(ln: LnCurrencyUnit): Boolean = {
    toPicoBitcoinValue <= ln.toPicoBitcoinValue
  }

  def !=(ln: LnCurrencyUnit): Boolean = !(this == ln)

  def ==(ln: LnCurrencyUnit): Boolean =
    toPicoBitcoinValue == ln.toPicoBitcoinValue

  override def +(ln: LnCurrencyUnit): LnCurrencyUnit = {
    PicoBitcoins(toPicoBitcoinValue + ln.toPicoBitcoinValue)
  }

  override def -(ln: LnCurrencyUnit): LnCurrencyUnit = {
    PicoBitcoins(toPicoBitcoinValue - ln.toPicoBitcoinValue)
  }

  override def *(factor: BigInt): LnCurrencyUnit =
    PicoBitcoins(toPicoBitcoinValue * factor)

  override def *(ln: LnCurrencyUnit): LnCurrencyUnit = {
    PicoBitcoins(toPicoBitcoinValue * ln.toPicoBitcoinValue)
  }

  def unary_- : LnCurrencyUnit = {
    PicoBitcoins(-toPicoBitcoinValue)
  }

  override def bytes: ByteVector = Int64(toPicoBitcoinValue).bytes.reverse

  def toUInt5s: Vector[UInt5] = {
    val u5s = Bech32.from8bitTo5bit(bytes)
    u5s
  }

  def toBigInt: BigInt

  def toLong: Long = toBigInt.bigInteger.longValueExact()

  def toInt: Int = toBigInt.bigInteger.intValueExact()

  protected def underlying: BigInt

  def toSatoshis: Satoshis = {
    LnCurrencyUnits.toSatoshi(this)
  }

  def toPicoBitcoinValue: BigInt = {
    toBigInt * toPicoBitcoinMultiplier
  }

  def toPicoBitcoinDecimal: BigDecimal = {
    BigDecimal(toPicoBitcoinValue.bigInteger)
  }

  def toPicoBitcoinMultiplier: Int

  def toPicoBitcoins: PicoBitcoins = PicoBitcoins(toPicoBitcoinValue)

  def encodedBytes: ByteVector = {
    ByteVector(toEncodedString.map(_.toByte))
  }

  def toMSat: MilliSatoshis = MilliSatoshis.fromPico(toPicoBitcoins)

  /** This returns the string encoding defined in BOLT11
    * For instance, 100
    * [[org.bitcoins.core.protocol.ln.currency.PicoBitcoins PicoBitcoins]]
    * would appear as "100p"
    */
  def toEncodedString: String = {
    toBigInt + character.toString
  }
}

sealed abstract class MilliBitcoins extends LnCurrencyUnit {
  override def character: Char = 'm'

  override def toPicoBitcoinMultiplier: Int = 1000000000

  override def toBigInt: BigInt = underlying

}

object MilliBitcoins extends BaseNumbers[MilliBitcoins] {
  val min = MilliBitcoins(LnPolicy.minMilliBitcoins)
  val max = MilliBitcoins(LnPolicy.maxMilliBitcoins)
  val zero = MilliBitcoins(0)
  val one = MilliBitcoins(1)

  def apply(milliBitcoins: Int64): MilliBitcoins =
    MilliBitcoins(milliBitcoins.toBigInt)

  def apply(underlying: BigInt): MilliBitcoins = MilliBitcoinsImpl(underlying)

  private case class MilliBitcoinsImpl(underlying: BigInt)
      extends MilliBitcoins {
    require(underlying >= LnPolicy.minMilliBitcoins,
            "Number was too small for MilliBitcoins, got: " + underlying)
    require(underlying <= LnPolicy.maxMilliBitcoins,
            "Number was too big for MilliBitcoins, got: " + underlying)

    override def toString: String =
      s"${underlying / toPicoBitcoinMultiplier} mBTC"
  }
}

sealed abstract class MicroBitcoins extends LnCurrencyUnit {
  override def character: Char = 'u'

  override def toPicoBitcoinMultiplier: Int = 1000000

  override def toBigInt: BigInt = underlying

}

object MicroBitcoins extends BaseNumbers[MicroBitcoins] {
  val min = MicroBitcoins(LnPolicy.minMicroBitcoins)
  val max = MicroBitcoins(LnPolicy.maxMicroBitcoins)
  val zero = MicroBitcoins(0)
  val one = MicroBitcoins(1)

  def apply(microBitcoins: Int64): MicroBitcoins =
    MicroBitcoins(microBitcoins.toBigInt)

  def apply(underlying: BigInt): MicroBitcoins = MicroBitcoinsImpl(underlying)

  private case class MicroBitcoinsImpl(underlying: BigInt)
      extends MicroBitcoins {
    require(underlying >= LnPolicy.minMicroBitcoins,
            "Number was too small for MicroBitcoins, got: " + underlying)
    require(underlying <= LnPolicy.maxMicroBitcoins,
            "Number was too big for MicroBitcoins, got: " + underlying)

    override def toString: String =
      s"${underlying / toPicoBitcoinMultiplier} uBTC"
  }
}

sealed abstract class NanoBitcoins extends LnCurrencyUnit {
  override def character: Char = 'n'

  override def toPicoBitcoinMultiplier: Int = 1000

  override def toBigInt: BigInt = underlying

}

object NanoBitcoins extends BaseNumbers[NanoBitcoins] {
  val min = NanoBitcoins(LnPolicy.minNanoBitcoins)
  val max = NanoBitcoins(LnPolicy.maxNanoBitcoins)
  val zero = NanoBitcoins(0)
  val one = NanoBitcoins(1)

  def apply(nanoBitcoins: Int64): NanoBitcoins =
    NanoBitcoins(nanoBitcoins.toBigInt)

  def apply(underlying: BigInt): NanoBitcoins = NanoBitcoinsImpl(underlying)

  private case class NanoBitcoinsImpl(underlying: BigInt) extends NanoBitcoins {
    require(underlying >= LnPolicy.minNanoBitcoins,
            "Number was too small for NanoBitcoins, got: " + underlying)
    require(underlying <= LnPolicy.maxNanoBitcoins,
            "Number was too big for NanoBitcoins, got: " + underlying)

    override def toString: String =
      s"${underlying / toPicoBitcoinMultiplier} nBTC"
  }
}

sealed abstract class PicoBitcoins extends LnCurrencyUnit {
  override def character: Char = 'p'

  override def toPicoBitcoinMultiplier: Int = 1

  override def toBigInt: BigInt = underlying
}

object PicoBitcoins extends BaseNumbers[PicoBitcoins] {
  val min = PicoBitcoins(LnPolicy.minPicoBitcoins)
  val max = PicoBitcoins(LnPolicy.maxPicoBitcoins)
  val zero = PicoBitcoins(0)
  val one = PicoBitcoins(1)

  def apply(i64: Int64): PicoBitcoins = PicoBitcoins(i64.toBigInt)

  def apply(underlying: BigInt): PicoBitcoins = PicoBitcoinsImpl(underlying)

  private case class PicoBitcoinsImpl(underlying: BigInt) extends PicoBitcoins {
    require(underlying >= LnPolicy.minPicoBitcoins,
            "Number was too small for PicoBitcoins, got: " + underlying)
    require(underlying <= LnPolicy.maxPicoBitcoins,
            "Number was too big for PicoBitcoins, got: " + underlying)

    override def toString: String = s"$toBigInt pBTC"
  }
}

object LnCurrencyUnits {
  private[currency] val PICO_TO_SATOSHIS = 10000
  private[currency] val MSAT_TO_PICO = 10
  val zero: LnCurrencyUnit = PicoBitcoins.zero

  /**
    * For information regarding the rounding of sub-Satoshi values, see
    * [[https://github.com/lightningnetwork/lightning-rfc/blob/master/03-transactions.md#commitment-transaction-outputs BOLT3]]
    */
  def toSatoshi(lnCurrencyUnits: LnCurrencyUnit): Satoshis = {
    val pico = lnCurrencyUnits.toPicoBitcoins
    val sat = pico.toBigInt / PICO_TO_SATOSHIS
    Satoshis(Int64(sat))
  }

  def fromMSat(msat: MilliSatoshis): PicoBitcoins = {
    //msat are technically 10^-11
    //while pico are 10^-12, so we need to convert
    val underlying = msat.toBigInt * MSAT_TO_PICO
    PicoBitcoins(underlying)
  }

  def fromEncodedString(input: String): Try[LnCurrencyUnit] = {
    val (amountStr, unit) = input.splitAt(input.length - 1)
    val amount = Try(BigInt(amountStr))
    if (amount.isSuccess) {
      unit match {
        case "m" => Try(MilliBitcoins(amount.get))
        case "u" => Try(MicroBitcoins(amount.get))
        case "n" => Try(NanoBitcoins(amount.get))
        case "p" => Try(PicoBitcoins(amount.get))
        case _: String =>
          Failure(new IllegalArgumentException(
            s"LnCurrencyUnit not found. Expected MilliBitcoins (m), MicroBitcoins (u), NanoBitcoins (n), or PicoBitcoins (p), got: $unit"))
      }
    } else {
      Failure(
        new IllegalArgumentException(
          s"Could not convert amount to valid number, got: $amount"))
    }
  }
}
