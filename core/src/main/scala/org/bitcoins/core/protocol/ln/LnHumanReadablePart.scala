package org.bitcoins.core.protocol.ln

import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.protocol.NetworkElement
import org.bitcoins.core.protocol.ln.LnParams._
import scodec.bits.ByteVector

import scala.util.matching.Regex
import scala.util.{ Failure, Success, Try }

sealed abstract class LnHumanReadablePart extends NetworkElement {
  def network: LnParams

  def amount: Option[LnCurrencyUnit]

  def networkToString: String = network match {
    case LnBitcoinMainNet => "lnbc"
    case LnBitcoinTestNet => "lntb"
    case LnBitcoinRegTest => "lnbcrt"
  }

  override def toString: String = {
    if (amount.isDefined) { networkToString + amount.get.toEncodedString }
    else networkToString
  }

  override def bytes: ByteVector = ByteVector(this.toString.map(n => n.toByte))
}

case class HumanReadablePart(lnParams: LnParams, amnt: Option[LnCurrencyUnit]) extends LnHumanReadablePart {
  require(amnt.isEmpty || amnt.get.toBigInt > 0, s"Invoice amount must be greater then 0, got $amount")
  require(amnt.isEmpty || amnt.get <= LnPolicy.maxAmountMSat, s"Invoice amount must be less than ${LnPolicy.maxAmountMSat}, got $amount")

  def network: LnParams = lnParams

  def amount: Option[LnCurrencyUnit] = amnt
}

case object lnbc extends LnHumanReadablePart {
  def network: LnParams = LnBitcoinMainNet
  def amount: Option[LnCurrencyUnit] = None
}

case object lntb extends LnHumanReadablePart {
  def network: LnParams = LnBitcoinTestNet
  def amount: Option[LnCurrencyUnit] = None
}

case object lnbcrt extends LnHumanReadablePart {
  def network: LnParams = LnBitcoinRegTest
  def amount: Option[LnCurrencyUnit] = None
}

object LnHumanReadablePart {

  private val allHRPs: Vector[LnHumanReadablePart] = Vector(lnbc, lntb, lnbcrt)

  private def networkFromString(prefix: String): Try[LnParams] = {
    val hrp = allHRPs.find(_.networkToString == prefix)
    if (hrp.isDefined) { Success(hrp.get.network) }
    else { Failure(new IllegalArgumentException(s"Unable to find a matching network, expected 'lnbc', 'lntb, or 'lnbcrt', got: $prefix")) }
  }

  def apply(network: NetworkParameters): Option[LnHumanReadablePart] = {
    val lnNetwork = LnParams.fromNetworkParameters(network)
    lnNetwork.map { params => HumanReadablePart(params, None) }
  }

  def apply(network: NetworkParameters, amount: LnCurrencyUnit): Option[LnHumanReadablePart] = {
    val lnNetwork = LnParams.fromNetworkParameters(network)
    lnNetwork.map { params => HumanReadablePart(params, Some(amount)) }
  }

  def apply(network: LnParams): LnHumanReadablePart = {
    HumanReadablePart(network, None)
  }

  def apply(network: LnParams, amount: LnCurrencyUnit): LnHumanReadablePart = {
    HumanReadablePart(network, Some(amount))
  }

  def apply(input: String): Try[LnHumanReadablePart] = {
    val networkPattern: Regex = "^[a-z]*".r //Select all of the letters, until we hit a number, as the network
    val networkString = networkPattern.findFirstIn(input).getOrElse("")
    val amountString = input.substring(networkString.length) //Select the remaining part of the string as the amount

    val lnNetwork = networkFromString(networkString)
    val amount = LnCurrencyUnits.fromEncodedString(amountString).toOption

    //If we are able to parse something as an amount, but are unable to convert it to a LnCurrencyUnit, we should fail.
    if (amount.isEmpty && !amountString.isEmpty) { Failure(new IllegalArgumentException(s"Parsed an amount, but could not convert to a valid currency, got: $amountString")) }
    else { lnNetwork.map { params => HumanReadablePart(params, amount) } }
  }
}