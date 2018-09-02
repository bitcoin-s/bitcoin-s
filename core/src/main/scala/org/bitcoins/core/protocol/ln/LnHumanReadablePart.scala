package org.bitcoins.core.protocol.ln

import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.protocol.NetworkElement
import org.bitcoins.core.protocol.ln.LnParams._
import scodec.bits.ByteVector
import scala.util.matching.Regex
import scala.util.{ Failure, Try }

sealed abstract class LnHumanReadablePart extends NetworkElement {
  def network: LnParams

  def amount: Option[LnCurrencyUnit]

  override def toString: String = network.value + invoiceAmount

  def invoiceAmount: String = {
    if (amount.isDefined) {
      amount.get.toBigInt.toString() + amount.get.character
    } else ""
  }

  override def bytes: ByteVector = ByteVector(network.value.map(c => c.toByte) ++ invoiceAmount.map(c => c.toByte)) //TODO: Verify
}

case class HumanReadablePart(inNetwork: LnParams, amnt: Option[LnCurrencyUnit]) extends LnHumanReadablePart {
  def network: LnParams = inNetwork
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
  def apply(network: NetworkParameters): Option[LnHumanReadablePart] = {
    val lnNetwork = LnParams.fromNetworkParameters(network)
    if (lnNetwork.isDefined) {
      Some(HumanReadablePart(lnNetwork.get, None))
    } else { None }
  }

  def apply(network: NetworkParameters, amount: LnCurrencyUnit): Option[LnHumanReadablePart] = {
    val lnNetwork = LnParams.fromNetworkParameters(network)
    if (lnNetwork.isDefined) {
      Some(HumanReadablePart(lnNetwork.get, Some(amount)))
    } else { None }
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
    val lnParam = LnParams.fromString(networkString)

    if (lnParam.isDefined) {
      if (amountString.isEmpty) {
        Try(apply(lnParam.get.network).get)
      } else {
        val amount = LnCurrencyUnits.fromEncodedString(amountString)
        Try(apply(lnParam.get.network, amount.get).get)
      }
    } else { Failure(new IllegalArgumentException(s"Could not convert to valid network. Expected MainNet (lnbc), TestNet3 (lntb), or RegTest (lnbcrt), got: $networkString")) }
  }
}