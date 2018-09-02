package org.bitcoins.core.protocol.ln

import org.bitcoins.core.config.{ MainNet, NetworkParameters, RegTest, TestNet3 }
import org.bitcoins.core.protocol.ln.LightningNetworkPrefix._

sealed abstract class LnHumanReadablePart {
  def network: LightningNetworkParams

  def amount: Option[LnCurrencyUnit]

  override def toString: String = network.value + invoiceAmount

  def invoiceAmount: String = {
    if (amount.isDefined) {
      amount.get.toBigInt.toString() + amount.get.character
    } else ""
  }

  def bytes: Seq[Byte] = network.value.map(c => c.toByte) ++ invoiceAmount.map(c => c.toByte)
}

case class HumanReadablePart(inNetwork: LightningNetworkParams, amnt: LnCurrencyUnit) extends LnHumanReadablePart {
  def network: LightningNetworkParams = inNetwork
  def amount: Option[LnCurrencyUnit] = Some(amnt)
}

case object lnbc extends LnHumanReadablePart {
  def network: LightningNetworkParams = LnBitcoinMainNet
  def amount: Option[LnCurrencyUnit] = None
}

case object lntb extends LnHumanReadablePart {
  def network: LightningNetworkParams = LnBitcoinTestNet
  def amount: Option[LnCurrencyUnit] = None
}

case object lnbcrt extends LnHumanReadablePart {
  def network: LightningNetworkParams = LnBitcoinRegTest
  def amount: Option[LnCurrencyUnit] = None
}

object LnHumanReadablePart {
  def apply(str: String): LnHumanReadablePart = str match {
    case "lnbc" => lnbc
    case "lntb" => lntb
    case "lnbcrt" => lnbcrt
    case _ => throw new IllegalArgumentException(s"Expected MainNet (lnbc), TestNet3 (lntb), or RegTestNet (lnbcrt), got $str")
  }

  def apply(network: NetworkParameters): LnHumanReadablePart = network match {
    case MainNet => lnbc
    case TestNet3 => lntb
    case RegTest => lnbcrt
    case _ => throw new IllegalArgumentException(s"Expected MainNet, TestNet3, or RegTestNet, got $network")
  }

  def apply(network: LightningNetworkParams, amnt: LnCurrencyUnit): LnHumanReadablePart = {
    HumanReadablePart(network, amnt)
  }

  def apply(network: NetworkParameters, amnt: LnCurrencyUnit): LnHumanReadablePart = {
    val lnNetwork = LightningNetworkPrefix.fromNetworkParameters(network)
    if (lnNetwork.isDefined) {
      HumanReadablePart(lnNetwork.get, amnt)
    } else { throw new IllegalArgumentException(s"Expected NetworkParameter, got $network")}
  }
}