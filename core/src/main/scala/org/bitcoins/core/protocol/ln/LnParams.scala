package org.bitcoins.core.protocol.ln

import org.bitcoins.core.config.{ MainNet, NetworkParameters, RegTest, TestNet3 }
import org.bitcoins.core.protocol.blockchain.ChainParams
import scodec.bits.ByteVector

sealed abstract class LnParams {
  def chain: ChainParams = network.chainParams

  def network: NetworkParameters

  def lnRpcPort: Int

  def lnPort: Int

  /**
   * The prefix for generating invoices for a Lightning Invoice. See
   * [[https://github.com/lightningnetwork/lightning-rfc/blob/master/11-payment-encoding.md BOLT11]]
   * for more details
   */
  def invoicePrefix: ByteVector
}

object LnParams {

  case object LnBitcoinMainNet extends LnParams {
    override def network: MainNet.type = MainNet

    override def lnRpcPort = 8080

    override def lnPort = 9735

    override val invoicePrefix: ByteVector = {
      ByteVector('l', 'n', 'b', 'c')
    }
  }

  case object LnBitcoinTestNet extends LnParams {
    override def network: TestNet3.type = TestNet3

    override def lnRpcPort = 8080

    override def lnPort = 9735

    override val invoicePrefix: ByteVector = {
      ByteVector('l', 'n', 't', 'b')
    }
  }

  case object LnBitcoinRegTest extends LnParams {
    override def network: RegTest.type = RegTest

    override def lnRpcPort = 8080

    override def lnPort = 9735

    override val invoicePrefix: ByteVector = {
      ByteVector('l', 'n', 'b', 'c', 'r', 't')
    }
  }

  def fromNetworkParameters(np: NetworkParameters): LnParams = np match {
    case MainNet => LnBitcoinMainNet
    case TestNet3 => LnBitcoinTestNet
    case RegTest => LnBitcoinRegTest
  }

  private val allNetworks: Vector[LnParams] = Vector(LnBitcoinMainNet, LnBitcoinTestNet, LnBitcoinRegTest)

  private val prefixes: Map[String, LnParams] = {
    val vec: Vector[(String, LnParams)] = {
      allNetworks.map { network =>
        (network.invoicePrefix.decodeAscii.right.get, network)
      }
    }
    vec.toMap
  }

  /**
   * Returns a [[org.bitcoins.core.protocol.ln.LnParams LnParams]] whose
   * network prefix matches the given string. See [[https://github.com/lightningnetwork/lightning-rfc/blob/master/11-payment-encoding.md#human-readable-part BOLT11 ]]
   * for more details on prefixes.
   */
  def fromPrefixString(str: String): Option[LnParams] = {
    prefixes.get(str)
  }
}