package org.bitcoins.core.protocol.ln.currency

import org.bitcoins.core.number.BaseNumbers

import scala.math.BigDecimal.RoundingMode

sealed abstract class MilliSatoshis {
  protected def underlying: BigInt

  def toBigInt: BigInt = underlying

  def toLong: Long = toBigInt.bigInteger.longValueExact()

  def toBigDecimal: BigDecimal = BigDecimal(toBigInt)

  def toLnCurrencyUnit: LnCurrencyUnit = {
    val underlying = toBigInt * BigInt(10)
    PicoBitcoins(underlying)
  }

  def ==(lnCurrencyUnit: LnCurrencyUnit): Boolean = {
    toLnCurrencyUnit == lnCurrencyUnit
  }

  def !=(lnCurrencyUnit: LnCurrencyUnit): Boolean = {
    toLnCurrencyUnit != lnCurrencyUnit
  }

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
    // we need to divide by 10
    val msatDec = pico / BigDecimal(10)

    //now we need to round, we are going to round the same way round
    //outputs when publishing txs to the blockchain
    //https://github.com/lightningnetwork/lightning-rfc/blob/master/03-transactions.md#commitment-transaction-outputs

    val rounded = msatDec.setScale(0, RoundingMode.DOWN)

    MilliSatoshis(rounded.toBigIntExact().get)
  }

  def apply(lnCurrencyUnit: LnCurrencyUnit): MilliSatoshis = {
    fromPico(picoBitcoins = lnCurrencyUnit.toPicoBitcoins)
  }
}
