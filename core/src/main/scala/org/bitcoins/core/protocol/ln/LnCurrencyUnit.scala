package org.bitcoins.core.protocol.ln

import org.bitcoins.core.currency.{ Bitcoins, Satoshis }
import org.bitcoins.core.number.{ BaseNumbers, Int64, UInt5, UInt8 }
import org.bitcoins.core.protocol.NetworkElement
import org.bitcoins.core.util.Bech32
import scodec.bits.ByteVector

import scala.math.BigDecimal.RoundingMode
import scala.util.{ Failure, Success, Try }

sealed abstract class LnCurrencyUnit extends NetworkElement {
  type A

  def character: Char

  def multiplier: BigDecimal

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

  def ==(ln: LnCurrencyUnit): Boolean = toPicoBitcoinValue == ln.toPicoBitcoinValue

  def +(ln: LnCurrencyUnit): LnCurrencyUnit = {
    PicoBitcoins(toPicoBitcoinValue + ln.toPicoBitcoinValue)
  }

  def -(ln: LnCurrencyUnit): LnCurrencyUnit = {
    PicoBitcoins(toPicoBitcoinValue - ln.toPicoBitcoinValue)
  }

  def *(ln: LnCurrencyUnit): LnCurrencyUnit = {
    PicoBitcoins(toPicoBitcoinValue * ln.toPicoBitcoinValue)
  }

  def unary_- : LnCurrencyUnit = {
    PicoBitcoins(-toPicoBitcoinValue)
  }

  override def bytes: ByteVector = Int64(toPicoBitcoinValue).bytes.reverse

  def fiveBitEncoding: Vector[UInt5] = {
    val u5s = Bech32.from8bitTo5bit(bytes)
    u5s
  }

  def toBigInt: BigInt

  def toLong: Long = toBigInt.toLong

  def toInt: Int = {
    require(this.toBigInt >= Int.MinValue, "Number was too small for Int, got: " + underlying)
    require(this.toBigInt <= Int.MaxValue, "Number was too big for Int, got: " + underlying)
    toBigInt.toInt
  }

  protected def underlying: A

  def toSatoshis: Satoshis

  def toPicoBitcoinValue: BigInt

  def toPicoBitcoinMultiplier: Int

  def encodedBytes: ByteVector = {
    ByteVector(toEncodedString.map(_.toByte))
  }
  def toEncodedString: String = {
    toBigInt + character.toString()
  }
}

sealed abstract class MilliBitcoins extends LnCurrencyUnit {
  override type A = BigInt

  override def character: Char = 'm'

  override def multiplier: BigDecimal = LnPolicy.milliMultiplier

  override def toPicoBitcoinMultiplier: Int = 1000000000

  override def toBigInt: A = underlying

  override def toSatoshis: Satoshis = LnCurrencyUnits.toSatoshi(this)

  override def toPicoBitcoinValue: BigInt = LnCurrencyUnits.toPicoBitcoinValue(this)
}

object MilliBitcoins extends BaseNumbers[MilliBitcoins] {
  val min = MilliBitcoins(LnPolicy.minMilliBitcoins)
  val max = MilliBitcoins(LnPolicy.maxMilliBitcoins)
  val zero = MilliBitcoins(0)
  val one = MilliBitcoins(1)

  def apply(milliBitcoins: Int64): MilliBitcoins = MilliBitcoins(milliBitcoins.toBigInt)

  def apply(underlying: BigInt): MilliBitcoins = MilliBitcoinsImpl(underlying)

  private case class MilliBitcoinsImpl(underlying: BigInt) extends MilliBitcoins {
    require(underlying >= LnPolicy.minMilliBitcoins, "Number was too small for MilliBitcoins, got: " + underlying)
    require(underlying <= LnPolicy.maxMilliBitcoins, "Number was too big for MilliBitcoins, got: " + underlying)
  }
}

sealed abstract class MicroBitcoins extends LnCurrencyUnit {
  override type A = BigInt

  override def character: Char = 'u'

  override def multiplier: BigDecimal = LnPolicy.microMultiplier

  override def toPicoBitcoinMultiplier: Int = 1000000

  override def toBigInt: A = underlying

  override def toSatoshis: Satoshis = LnCurrencyUnits.toSatoshi(this)

  override def toPicoBitcoinValue: BigInt = LnCurrencyUnits.toPicoBitcoinValue(this)
}

object MicroBitcoins extends BaseNumbers[MicroBitcoins] {
  val min = MicroBitcoins(LnPolicy.minMicroBitcoins)
  val max = MicroBitcoins(LnPolicy.maxMicroBitcoins)
  val zero = MicroBitcoins(0)
  val one = MicroBitcoins(1)

  def apply(microBitcoins: Int64): MicroBitcoins = MicroBitcoins(microBitcoins.toBigInt)

  def apply(underlying: BigInt): MicroBitcoins = MicroBitcoinsImpl(underlying)

  private case class MicroBitcoinsImpl(underlying: BigInt) extends MicroBitcoins {
    require(underlying >= LnPolicy.minMicroBitcoins, "Number was too small for MicroBitcoins, got: " + underlying)
    require(underlying <= LnPolicy.maxMicroBitcoins, "Number was too big for MicroBitcoins, got: " + underlying)
  }
}

sealed abstract class NanoBitcoins extends LnCurrencyUnit {
  override type A = BigInt

  override def character: Char = 'n'

  override def multiplier: BigDecimal = LnPolicy.nanoMultiplier

  override def toPicoBitcoinMultiplier: Int = 1000

  override def toBigInt: A = underlying

  override def toSatoshis: Satoshis = LnCurrencyUnits.toSatoshi(this)

  override def toPicoBitcoinValue: BigInt = LnCurrencyUnits.toPicoBitcoinValue(this)
}

object NanoBitcoins extends BaseNumbers[NanoBitcoins] {
  val min = NanoBitcoins(LnPolicy.minNanoBitcoins)
  val max = NanoBitcoins(LnPolicy.maxNanoBitcoins)
  val zero = NanoBitcoins(0)
  val one = NanoBitcoins(1)

  def apply(nanoBitcoins: Int64): NanoBitcoins = NanoBitcoins(nanoBitcoins.toBigInt)

  def apply(underlying: BigInt): NanoBitcoins = NanoBitcoinsImpl(underlying)

  private case class NanoBitcoinsImpl(underlying: BigInt) extends NanoBitcoins {
    require(underlying >= LnPolicy.minNanoBitcoins, "Number was too small for NanoBitcoins, got: " + underlying)
    require(underlying <= LnPolicy.maxNanoBitcoins, "Number was too big for NanoBitcoins, got: " + underlying)
  }
}

sealed abstract class PicoBitcoins extends LnCurrencyUnit {
  override type A = BigInt

  override def character: Char = 'p'

  override def multiplier: BigDecimal = LnPolicy.picoMultiplier

  override def toPicoBitcoinMultiplier: Int = 1

  override def toBigInt: A = underlying

  override def toSatoshis: Satoshis = LnCurrencyUnits.toSatoshi(this)

  override def toPicoBitcoinValue: BigInt = this.toBigInt
}

object PicoBitcoins extends BaseNumbers[PicoBitcoins] {
  val min = PicoBitcoins(LnPolicy.minPicoBitcoins)
  val max = PicoBitcoins(LnPolicy.maxPicoBitcoins)
  val zero = PicoBitcoins(0)
  val one = PicoBitcoins(1)

  def apply(i64: Int64): PicoBitcoins = PicoBitcoins(i64.toBigInt)

  def apply(underlying: BigInt): PicoBitcoins = PicoBitcoinsImpl(underlying)

  private case class PicoBitcoinsImpl(underlying: BigInt) extends PicoBitcoins {
    require(underlying >= LnPolicy.minPicoBitcoins, "Number was too small for PicoBitcoins, got: " + underlying)
    require(underlying <= LnPolicy.maxPicoBitcoins, "Number was too big for PicoBitcoins, got: " + underlying)
  }
}

object LnCurrencyUnits {
  val oneMilliBTC: LnCurrencyUnit = MilliBitcoins.one
  val oneMicroBTC: LnCurrencyUnit = MicroBitcoins.one
  val oneNanoBTC: LnCurrencyUnit = NanoBitcoins.one
  val onePicoBTC: LnCurrencyUnit = PicoBitcoins.one
  val zero = PicoBitcoins(0)

  val milliMultiplier: BigDecimal = LnPolicy.milliMultiplier
  val microMultiplier: BigDecimal = LnPolicy.microMultiplier
  val nanoMultiplier: BigDecimal = LnPolicy.nanoMultiplier
  val picoMultiplier: BigDecimal = LnPolicy.picoMultiplier

  def toPicoBitcoinValue(lnCurrencyUnits: LnCurrencyUnit): BigInt = {
    lnCurrencyUnits.toBigInt * lnCurrencyUnits.toPicoBitcoinMultiplier
  }

  /**
   * For information regarding the rounding of sub-Satoshi values, please visit:
   * https://github.com/lightningnetwork/lightning-rfc/blob/master/03-transactions.md#commitment-transaction-outputs
   */
  def toSatoshi(lnCurrencyUnits: LnCurrencyUnit): Satoshis = {
    val sat = BigDecimal(lnCurrencyUnits.toBigInt) * Bitcoins.one.satoshis.toBigDecimal * lnCurrencyUnits.multiplier
    val rounded = sat.setScale(0, RoundingMode.DOWN)
    if (rounded >= 1) {
      Satoshis(Int64(rounded.toBigIntExact().get))
    } else Satoshis.zero
  }

  def fromEncodedString(input: String): Try[LnCurrencyUnit] = {
    val currency = input.splitAt(input.length - 1)
    val amount = Try(BigInt(currency._1))
    if (amount.isSuccess) {
      val unit = currency._2
      unit match {
        case "m" => Try(MilliBitcoins(amount.get))
        case "u" => Try(MicroBitcoins(amount.get))
        case "n" => Try(NanoBitcoins(amount.get))
        case "p" => Try(PicoBitcoins(amount.get))
        case _ => Failure(new IllegalArgumentException(s"LnCurrencyUnit not found. Expected MilliBitcoins (m), MicroBitcoins (u), NanoBitcoins (n), or PicoBitcoins (p), got: $unit"))
      }
    } else { Failure(new IllegalArgumentException(s"Could not convert amount to valid number, got: $amount")) }
  }
}