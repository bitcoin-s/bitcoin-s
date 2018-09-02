package org.bitcoins.core.protocol.ln

import org.bitcoins.core.config.{ MainNet, NetworkParameters, RegTest, TestNet3 }

sealed abstract class LightningNetworkParams {
  def value: String
  def network: NetworkParameters
}

object LightningNetworkPrefix {

  case object LnBitcoinMainNet extends LightningNetworkParams {
    override def value = "lnbc"
    override def network = MainNet
  }

  case object LnBitcoinTestNet extends LightningNetworkParams {
    override def value = "lntb"
    override def network = TestNet3
  }

  case object LnBitcoinRegTest extends LightningNetworkParams {
    override def value = "lnbcrt"
    override def network = RegTest
  }

  val allNetworks: Vector[LightningNetworkParams] = Vector(LnBitcoinMainNet, LnBitcoinTestNet, LnBitcoinRegTest)

  def fromString(prefix: String): Option[LightningNetworkParams] = {
    allNetworks.find(_.value == prefix)
  }

  def fromNetworkParameters(np: NetworkParameters): Option[LightningNetworkParams] = {
    allNetworks.find(_.network == np)
  }
}