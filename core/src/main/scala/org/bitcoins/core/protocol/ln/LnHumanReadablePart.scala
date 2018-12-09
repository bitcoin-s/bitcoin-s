package org.bitcoins.core.protocol.ln

import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.protocol.ln.LnParams._
import org.bitcoins.core.protocol.ln.currency.{LnCurrencyUnit, LnCurrencyUnits}
import org.bitcoins.core.util.Bech32
import scodec.bits.ByteVector

import scala.util.matching.Regex
import scala.util.{Failure, Success, Try}

sealed abstract class LnHumanReadablePart {
  require(amount.isEmpty || amount.get.toBigInt > 0,
          s"Invoice amount must be greater then 0, got $amount")
  require(
    amount.isEmpty || amount.get.toMSat <= LnPolicy.maxAmountMSat,
    s"Invoice amount must not exceed ${LnPolicy.maxAmountMSat}, got ${amount.get.toMSat}")

  def network: LnParams

  def amount: Option[LnCurrencyUnit]

  def bytes: ByteVector = {
    network.invoicePrefix ++ amount
      .map(_.encodedBytes)
      .getOrElse(ByteVector.empty)
  }

  override def toString: String = {
    val b = StringBuilder.newBuilder
    val prefixChars = network.invoicePrefix.toArray.map(_.toChar)
    prefixChars.foreach(b.append)

    val amt = amount.map(_.toEncodedString).getOrElse("")
    b.append(amt)

    b.toString()
  }
}

object LnHumanReadablePart {

  /** Prefix for generating a LN invoice on the Bitcoin MainNet */
  case class lnbc(override val amount: Option[LnCurrencyUnit])
      extends LnHumanReadablePart {
    override def network: LnParams = LnBitcoinMainNet
  }

  /** Prefix for generating a LN invoice on the Bitcoin TestNet3 */
  case class lntb(override val amount: Option[LnCurrencyUnit])
      extends LnHumanReadablePart {
    override def network: LnParams = LnBitcoinTestNet
  }

  /** Prefix for genearting a LN invoice on the Bitcoin RegTest */
  case class lnbcrt(override val amount: Option[LnCurrencyUnit])
      extends LnHumanReadablePart {
    def network: LnParams = LnBitcoinRegTest
  }

  def apply(network: NetworkParameters): LnHumanReadablePart = {
    val lnNetwork = LnParams.fromNetworkParameters(network)
    LnHumanReadablePart.fromLnParams(lnNetwork)
  }

  def apply(
      network: NetworkParameters,
      amount: LnCurrencyUnit): LnHumanReadablePart = {
    val lnNetwork = LnParams.fromNetworkParameters(network)
    LnHumanReadablePart(lnNetwork, Some(amount))
  }

  def apply(network: LnParams): LnHumanReadablePart = {
    fromLnParams(network)
  }

  /**
    * Will return a [[org.bitcoins.core.protocol.ln.LnHumanReadablePart LnHumanReadablePart]]
    * without a [[org.bitcoins.core.protocol.ln.currency.LnCurrencyUnit LnCurrencyUnit]] encoded in the invoice
    */
  def fromLnParams(network: LnParams): LnHumanReadablePart = {
    LnHumanReadablePart(network, None)
  }

  /**
    * Will return a [[org.bitcoins.core.protocol.ln.LnHumanReadablePart LnHumanReadablePart]]
    * with the provide [[org.bitcoins.core.protocol.ln.currency.LnCurrencyUnit LnCurrencyUnit]] encoded in the invoice
    */
  def apply(
      network: LnParams,
      amount: Option[LnCurrencyUnit]): LnHumanReadablePart = {
    fromParamsAmount(network, amount)
  }

  def fromParamsAmount(
      network: LnParams,
      amount: Option[LnCurrencyUnit]): LnHumanReadablePart = {
    network match {
      case LnParams.LnBitcoinMainNet => lnbc(amount)
      case LnParams.LnBitcoinTestNet => lntb(amount)
      case LnParams.LnBitcoinRegTest => lnbcrt(amount)
    }
  }

  /**
    * First two chars MUST be 'ln'
    * Next chars must be the BIP173 currency prefixes. For more information, see
    * [[https://github.com/lightningnetwork/lightning-rfc/blob/master/11-payment-encoding.md#human-readable-part BOLT11]]
    * and
    * [[https://github.com/bitcoin/bips/blob/master/bip-0173.mediawiki#Specification BIP173]]
    */
  def fromString(input: String): Try[LnHumanReadablePart] = {
    val hrpIsValidT = Bech32.checkHrpValidity(input, parse)
    hrpIsValidT
  }

  private def parse(input: String): Try[LnHumanReadablePart] = {
    //Select all of the letters, until we hit a number, as the network
    val networkPattern: Regex = "^[a-z]*".r
    val networkStringOpt = networkPattern.findFirstIn(input)
    val lnParamsOpt = networkStringOpt.flatMap(LnParams.fromPrefixString(_))

    if (lnParamsOpt.isEmpty) {
      Failure(
        new IllegalArgumentException(
          s"Could not parse a valid network prefix, got ${input}"))
    } else {

      val lnParams = lnParamsOpt.get
      val prefixSize = lnParams.invoicePrefix.size.toInt
      val amountString = input.slice(prefixSize, input.size)
      val amount = LnCurrencyUnits.fromEncodedString(amountString).toOption

      //If we are able to parse something as an amount, but are unable to convert it to a LnCurrencyUnit, we should fail.
      if (amount.isEmpty && !amountString.isEmpty) {
        Failure(
          new IllegalArgumentException(
            s"Parsed an amount, " +
              s"but could not convert to a valid currency, got: $amountString"))
      } else {
        Success(LnHumanReadablePart(lnParams, amount))
      }
    }
  }

}
