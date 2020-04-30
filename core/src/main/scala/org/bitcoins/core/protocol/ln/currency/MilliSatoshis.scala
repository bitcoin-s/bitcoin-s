package org.bitcoins.core.protocol.ln.currency

import org.bitcoins.core.currency.{CurrencyUnit, Satoshis}
import org.bitcoins.core.number.{BasicArithmetic, UInt64}
import org.bitcoins.crypto.NetworkElement
import scodec.bits.ByteVector

import scala.math.BigDecimal.RoundingMode

/**
  * The common currency unit used in the
  * LN protocol for updating HTLCs.
  *
  * @see [[https://github.com/lightningnetwork/lightning-rfc/blob/master/02-peer-protocol.md#adding-an-htlc-update_add_htlc BOLT2]]
  */
sealed abstract class MilliSatoshis
    extends NetworkElement
    with Ordered[MilliSatoshis]
    with BasicArithmetic[MilliSatoshis] {
  require(toBigInt >= 0, s"Millisatoshis cannot be negative, got $toBigInt")

  protected def underlying: BigInt

  /**
    * Output example:
    * {{{
    * > MilliSatoshis(10)
    * 10 msat
    * }}}
    */
  override def toString: String = {
    val num = toBigInt
    val postFix = if (num == 1) "msat" else "msats"
    s"$num $postFix"
  }

  def toBigInt: BigInt = underlying

  def toLong: Long = toBigInt.bigInteger.longValueExact

  def toBigDecimal: BigDecimal = BigDecimal(toBigInt)

  def toLnCurrencyUnit: LnCurrencyUnit = {
    LnCurrencyUnits.fromMSat(this)
  }

  def ==(lnCurrencyUnit: LnCurrencyUnit): Boolean = {
    toLnCurrencyUnit == lnCurrencyUnit
  }

  def !=(lnCurrencyUnit: LnCurrencyUnit): Boolean = {
    toLnCurrencyUnit != lnCurrencyUnit
  }

  def >=(ln: LnCurrencyUnit): Boolean = {
    toLnCurrencyUnit >= ln
  }

  def >(ln: LnCurrencyUnit): Boolean = {
    toLnCurrencyUnit > ln
  }

  def <(ln: LnCurrencyUnit): Boolean = {
    toLnCurrencyUnit < ln
  }

  def <=(ln: LnCurrencyUnit): Boolean = {
    toLnCurrencyUnit <= ln
  }

  def ==(ms: MilliSatoshis): Boolean = {
    toBigInt == ms.toBigInt
  }

  def !=(ms: MilliSatoshis): Boolean = {
    toBigInt != ms.toBigInt
  }

  override def compare(ms: MilliSatoshis): Int = toBigInt compare ms.toBigInt

  override def +(ms: MilliSatoshis): MilliSatoshis = {
    MilliSatoshis(toBigInt + ms.toBigInt)
  }

  override def -(ms: MilliSatoshis): MilliSatoshis = {
    MilliSatoshis(toBigInt - ms.toBigInt)
  }

  override def *(factor: BigInt): MilliSatoshis = {
    MilliSatoshis(toBigInt * factor)
  }

  override def *(factor: MilliSatoshis): MilliSatoshis = {
    MilliSatoshis(toBigInt * factor.toBigInt)
  }

  def toUInt64: UInt64 = {
    UInt64(underlying)
  }

  def toSatoshis: Satoshis = {
    toLnCurrencyUnit.toSatoshis
  }

  override def bytes: ByteVector = toUInt64.bytes.reverse

}

object MilliSatoshis {

  private case class MilliSatoshisImpl(underlying: BigInt) extends MilliSatoshis

  val zero: MilliSatoshis = MilliSatoshis(0)
  val one: MilliSatoshis = MilliSatoshis(1)

  def apply(underlying: BigInt): MilliSatoshis = {
    MilliSatoshisImpl(underlying)
  }

  def fromPico(picoBitcoins: PicoBitcoins): MilliSatoshis = {
    val pico = picoBitcoins.toPicoBitcoinDecimal
    // we need to divide by 10 to get to msat
    val msatDec = pico / LnCurrencyUnits.MSAT_TO_PICO

    //now we need to round, we are going to round the same way round
    //outputs when publishing txs to the blockchain
    //https://github.com/lightningnetwork/lightning-rfc/blob/master/03-transactions.md#commitment-transaction-outputs

    val rounded = msatDec.setScale(0, RoundingMode.DOWN)

    MilliSatoshis(rounded.toBigIntExact.get)
  }

  def apply(lnCurrencyUnit: LnCurrencyUnit): MilliSatoshis = {
    fromPico(picoBitcoins = lnCurrencyUnit.toPicoBitcoins)
  }

  def apply(currencyUnit: CurrencyUnit): MilliSatoshis = {
    fromSatoshis(currencyUnit.satoshis)
  }

  def fromSatoshis(sat: Satoshis): MilliSatoshis = {
    MilliSatoshis(sat.toBigInt * 1000)
  }
}
