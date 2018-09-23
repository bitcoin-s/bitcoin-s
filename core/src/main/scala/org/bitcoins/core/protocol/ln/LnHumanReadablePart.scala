package org.bitcoins.core.protocol.ln

import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.number.{ UInt5, UInt8 }
import org.bitcoins.core.protocol.{ HumanReadablePart, NetworkElement }
import org.bitcoins.core.protocol.ln.LnParams._
import org.bitcoins.core.util.Bech32
import scodec.bits.ByteVector

import scala.annotation.tailrec
import scala.util.matching.Regex
import scala.util.{ Failure, Success, Try }

sealed abstract class LnHumanReadablePart {
  require(
    amount.isEmpty || amount.get.toBigInt > 0,
    s"Invoice amount must be greater then 0, got $amount")
  require(
    amount.isEmpty || amount.get <= LnPolicy.maxAmountMSat,
    s"Invoice amount must be less than ${LnPolicy.maxAmountMSat}, got $amount")

  def network: LnParams

  def amount: Option[LnCurrencyUnit]

  def bytes: ByteVector = {
    network.invoicePrefix ++ amount.map(_.encodedBytes).getOrElse(ByteVector.empty)
  }

  override def toString: String = {
    val b = StringBuilder.newBuilder
    val prefix = network.invoicePrefix.toArray.map(_.toChar).mkString
    b.append(prefix)

    val amt = amount.map(_.toEncodedString).getOrElse("")
    b.append(amt)

    b.toString()
  }
}

/** Prefix for generating a LN invoice on the Bitcoin MainNet */
case class lnbc(override val amount: Option[LnCurrencyUnit]) extends LnHumanReadablePart {
  override def network: LnParams = LnBitcoinMainNet
}

/** Prefix for generating a LN invoice on the Bitcoin TestNet3 */
case class lntb(override val amount: Option[LnCurrencyUnit]) extends LnHumanReadablePart {
  override def network: LnParams = LnBitcoinTestNet
}

/** Prefix for genearting a LN invoice on the Bitcoin RegTest */
case class lnbcrt(override val amount: Option[LnCurrencyUnit]) extends LnHumanReadablePart {
  def network: LnParams = LnBitcoinRegTest
}

object LnHumanReadablePart {

  def apply(network: NetworkParameters): Option[LnHumanReadablePart] = {
    val lnNetworkOpt = LnParams.fromNetworkParameters(network)
    lnNetworkOpt.map(LnHumanReadablePart.fromLnParams(_))
  }

  def apply(network: NetworkParameters, amount: LnCurrencyUnit): Option[LnHumanReadablePart] = {
    val lnNetworkOpt = LnParams.fromNetworkParameters(network)
    lnNetworkOpt.map(ln => LnHumanReadablePart(ln, Some(amount)))
  }

  def apply(network: LnParams): LnHumanReadablePart = {
    fromLnParams(network)
  }

  /**
   * Will return a [[org.bitcoins.core.protocol.ln.LnHumanReadablePart]]
   * without a [[LnCurrencyUnit]] encoded in the invoice
   * @param network
   * @return
   */
  def fromLnParams(network: LnParams): LnHumanReadablePart = {
    LnHumanReadablePart(network, None)
  }

  /**
   * Will return a [[org.bitcoins.core.protocol.ln.LnHumanReadablePart]]
   * with the provide [[LnCurrencyUnit]] encoded in the invoice
   * @param network
   * @param amount
   * @return
   */
  def apply(network: LnParams, amount: Option[LnCurrencyUnit]): LnHumanReadablePart = {
    network match {
      case LnParams.LnBitcoinMainNet => lnbc(amount)
      case LnParams.LnBitcoinTestNet => lntb(amount)
      case LnParams.LnBitcoinRegTest => lnbcrt(amount)
    }
  }

  /**
   * First two chars MUST be 'ln'
   * Next chars must be the BIP173 currency prefixes
   * [[https://github.com/lightningnetwork/lightning-rfc/blob/master/11-payment-encoding.md#human-readable-part]]
   * [[https://github.com/bitcoin/bips/blob/master/bip-0173.mediawiki#Specification]]
   * @param input
   * @return
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
      Failure(new IllegalArgumentException(s"Could not parse a valid network prefix, got ${input}"))
    } else {

      val lnParams = lnParamsOpt.get
      val prefixSize = lnParams.invoicePrefix.size.toInt
      val amountString = input.slice(prefixSize, input.size)
      val amount = LnCurrencyUnits.fromEncodedString(amountString).toOption

      //If we are able to parse something as an amount, but are unable to convert it to a LnCurrencyUnit, we should fail.
      if (amount.isEmpty && !amountString.isEmpty) {
        Failure(new IllegalArgumentException(s"Parsed an amount, " +
          s"but could not convert to a valid currency, got: $amountString"))
      } else {
        Success(LnHumanReadablePart(lnParams, amount))
      }
    }
  }

}