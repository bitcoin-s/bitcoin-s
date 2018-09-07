package org.bitcoins.core.protocol.ln

import org.bitcoins.core.config.{ MainNet, NetworkParameters, RegTest, TestNet3 }
import org.bitcoins.core.protocol.blockchain.ChainParams

sealed abstract class LnParams {
  def chain: ChainParams = network.chainParams

  def network: NetworkParameters

  def lnRpcPort: Int

  def lnPort: Int
}

object LnParams {

  case object LnBitcoinMainNet extends LnParams {
    override def network = MainNet

    override def lnRpcPort = 8080

    override def lnPort = 9735
  }

  case object LnBitcoinTestNet extends LnParams {
    override def network = TestNet3

    override def lnRpcPort = 8080

    override def lnPort = 9735
  }

  case object LnBitcoinRegTest extends LnParams {
    override def network = RegTest

    override def lnRpcPort = 8080

    override def lnPort = 9735
  }

  val allNetworks: Vector[LnParams] = Vector(LnBitcoinMainNet, LnBitcoinTestNet, LnBitcoinRegTest)

  def fromNetworkParameters(np: NetworkParameters): Option[LnParams] = {
    allNetworks.find(_.network == np)
  }
}