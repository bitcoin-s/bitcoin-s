package org.bitcoins.core.protocol.ln

import org.bitcoins.core.config.{ MainNet, NetworkParameters, RegTest, TestNet3 }

sealed abstract class LnParams {
  def value: String
  def network: NetworkParameters
}

object LnParams {

  case object LnBitcoinMainNet extends LnParams {
    override def value = "lnbc"
    override def network = MainNet
  }

  case object LnBitcoinTestNet extends LnParams {
    override def value = "lntb"
    override def network = TestNet3
  }

  case object LnBitcoinRegTest extends LnParams {
    override def value = "lnbcrt"
    override def network = RegTest
  }

  val allNetworks: Vector[LnParams] = Vector(LnBitcoinMainNet, LnBitcoinTestNet, LnBitcoinRegTest)

  def fromString(prefix: String): Option[LnParams] = {
    allNetworks.find(_.value == prefix)
  }

  def fromNetworkParameters(np: NetworkParameters): Option[LnParams] = {
    allNetworks.find(_.network == np)
  }
}