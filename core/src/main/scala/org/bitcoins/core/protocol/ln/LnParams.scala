package org.bitcoins.core.protocol.ln

import org.bitcoins.core.config.{ MainNet, NetworkParameters, RegTest, TestNet3 }
import org.bitcoins.core.number.UInt5
import org.bitcoins.core.protocol.blockchain.ChainParams
import scodec.bits.ByteVector

sealed abstract class LnParams {
  def chain: ChainParams = network.chainParams

  def network: NetworkParameters

  def lnRpcPort: Int

  def lnPort: Int

  /**
   * The prefix for generating invoices for a Lightning Invoice
   * See BOLT11 for more details
   * [[https://github.com/lightningnetwork/lightning-rfc/blob/master/11-payment-encoding.md]]
   * @return
   */
  def invoicePrefix: ByteVector
}

object LnParams {

  case object LnBitcoinMainNet extends LnParams {
    override def network = MainNet

    override def lnRpcPort = 8080

    override def lnPort = 9735

    override val invoicePrefix: ByteVector = {
      ByteVector('l', 'n', 'b', 'c')
    }
  }

  case object LnBitcoinTestNet extends LnParams {
    override def network = TestNet3

    override def lnRpcPort = 8080

    override def lnPort = 9735

    override val invoicePrefix: ByteVector = {
      ByteVector('l', 'n', 't', 'b')
    }
  }

  case object LnBitcoinRegTest extends LnParams {
    override def network = RegTest

    override def lnRpcPort = 8080

    override def lnPort = 9735

    override val invoicePrefix: ByteVector = {
      ByteVector('l', 'n', 'b', 'c', 'r', 't')
    }
  }

  private val allNetworks: Vector[LnParams] = Vector(LnBitcoinMainNet, LnBitcoinTestNet, LnBitcoinRegTest)

  def fromNetworkParameters(np: NetworkParameters): Option[LnParams] = {
    allNetworks.find(_.network == np)
  }

  private val prefixes: Map[String, LnParams] = {
    val vec: Vector[(String, LnParams)] = {
      allNetworks.map { network =>
        (network.invoicePrefix.decodeAscii.right.get, network)
      }
    }
    vec.toMap
  }

  /**
   * Returns a [[org.bitcoins.core.protocol.ln.LnParams]] whose
   * network prefix matches the given string. See BOLT11 for more details on prefixes
   * [[https://github.com/lightningnetwork/lightning-rfc/blob/master/11-payment-encoding.md#human-readable-part]]
   * @param str
   * @return
   */
  def fromPrefixString(str: String): Option[LnParams] = {
    prefixes.get(str)
  }
}