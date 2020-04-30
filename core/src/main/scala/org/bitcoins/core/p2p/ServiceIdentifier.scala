package org.bitcoins.core.p2p

import org.bitcoins.core.number.UInt64
import org.bitcoins.core.serializers.p2p.messages.RawServiceIdentifierSerializer
import org.bitcoins.crypto.{Factory, NetworkElement}
import scodec.bits.ByteVector

/**
  * Indicates the services that are provided by a node on the P2P network
  *
  * @see [[https://bitcoin.org/en/developer-reference#version]]
  * @see [[https://github.com/bitcoin/bitcoin/blob/master/src/protocol.h#L247 protocol.h]]
  *      in Bitcoin Core
  */
sealed abstract class ServiceIdentifier extends NetworkElement {

  /** The underlying number backing the set of flags */
  private[bitcoins] val num: UInt64
  private val reversedBits = num.bytes.bits.reverse

  override def bytes: ByteVector = RawServiceIdentifierSerializer.write(this)

  /**
    * This node is not a full node.
    * It may not be able to provide any data except for the transactions it originates.
    */
  lazy val nodeNone: Boolean = num == UInt64.zero

  /**
    * This is a full node and can be asked for full blocks.
    * It should implement all protocol features available in
    * its self-reported protocol version.
    */
  lazy val nodeNetwork: Boolean = reversedBits(0) // 1 << 0

  /**
    * This is a full node capable of responding to the
    * `getutxo` protocol request. This is not supported
    * by any currently-maintained Bitcoin node.
    *
    * @see [[https://github.com/bitcoin/bips/blob/master/bip-0064.mediawiki BIP64]]
    *      for details on how this is implemented.
    */
  lazy val nodeGetUtxo: Boolean = reversedBits(1) // 1 << 1

  /**
    * This is a full node capable and willing to handle
    * bloom-filtered connections.
    *
    * @see [[https://github.com/bitcoin/bips/blob/master/bip-0111.mediawiki BIP111]]
    *      for details
    */
  lazy val nodeBloom: Boolean = reversedBits(2) // 1 << 2

  /**
    * This is a full node that can be asked for blocks
    * and transactions including witness data.
    *
    * @see [[https://github.com/bitcoin/bips/blob/master/bip-0144.mediawiki BIP144]]]
    *      for details.
    */
  lazy val nodeWitness: Boolean = reversedBits(3) // 1 << 3

  /**
    * This is a full node that supports Xtreme Thinblocks.
    * This is not supported by any currently-maintained
    * Bitcoin node.
    */
  lazy val nodeXthin: Boolean = reversedBits(4) // 1 << 4

  /**
    * NODE_COMPACT_FILTERS means the node will service basic
    * block filter requests.
    *
    * @see [[https://github.com/bitcoin/bips/blob/master/bip-0157 BIP157]] and
    *   [https://github.com/bitcoin/bips/blob/master/bip-0158 BIP158]
    *   for details on how this is implemented.
    *
    * @note This is not yet supported by any Core release. Currently
    *       (aug. 1 2019) is a open PR by jimpo: https://github.com/bitcoin/bitcoin/pull/16442
    */
  lazy val nodeCompactFilters: Boolean = reversedBits(6) // 1 << 6

  /**
    * this means the same as `nodeNetwork` with the limitation of only
    * serving the last 288 (2 days) blocks
    *
    * @see [[https://github.com/bitcoin/bips/blob/master/bip-0159.mediawiki BIP159]]
    *      for details on how this is implemented.
    */
  lazy val nodeNetworkLimited: Boolean = reversedBits(10) // 1 << 10

  override def toString: String = {
    val innerText =
      if (nodeNone) "none"
      else
        s"network=$nodeNetwork, compactFilters=$nodeCompactFilters, getUtxo=$nodeGetUtxo, bloom=$nodeBloom, witness=$nodeWitness, xthin=$nodeXthin, networkLimited=$nodeNetworkLimited"
    s"ServiceIdentifier($innerText)"
  }
}

/**
  * Designated type for any service that does not have value of 0 or 1
  */
sealed trait UnknownService extends ServiceIdentifier {
  override def toString(): String = s"UnknownService(${num.toLong})"
}

object ServiceIdentifier extends Factory[ServiceIdentifier] {

  /**
    * This node is not a full node.
    * It may not be able to provide any data except for the transactions it originates.
    */
  val NODE_NONE = ServiceIdentifier(UInt64.zero)

  /**
    * This is a full node and can be asked for full blocks.
    * It should implement all protocol features available in its self-reported protocol version.
    */
  val NODE_NETWORK: ServiceIdentifier = ServiceIdentifier(1 << 0)

  /**
    * This is a full node capable of responding to the
    * `getutxo` protocol request. This is not supported
    * by any currently-maintained Bitcoin node.
    *
    * @see [[https://github.com/bitcoin/bips/blob/master/bip-0064.mediawiki BIP64]]
    *      for details on how this is implemented.
    */
  val NODE_GET_UTXO: ServiceIdentifier = ServiceIdentifier(1 << 1)

  /**
    * This is a full node capable and willing to handle
    * bloom-filtered connections.
    *
    * @see [[https://github.com/bitcoin/bips/blob/master/bip-0111.mediawiki BIP111]]
    *      for details
    */
  val NODE_BLOOM: ServiceIdentifier = ServiceIdentifier(1 << 2)

  /**
    * This is a full node that can be asked for blocks
    * and transactions including witness data.
    *
    * @see [[https://github.com/bitcoin/bips/blob/master/bip-0144.mediawiki BIP144]]]
    *      for details.
    */
  val NODE_WITNESS: ServiceIdentifier = ServiceIdentifier(1 << 3)

  /**
    * This is a full node that supports Xtreme Thinblocks.
    * This is not supported by any currently-maintained
    * Bitcoin node.
    */
  val NODE_XTHIN: ServiceIdentifier = ServiceIdentifier(1 << 4)

  val NODE_COMPACT_FILTERS = ServiceIdentifier(1 << 6)

  /**
    * This means the same as `NODE_NETWORK` with the limitation of only
    * serving the last 288 (2 days) blocks
    *
    * @see [[https://github.com/bitcoin/bips/blob/master/bip-0159.mediawiki BIP159]]
    *      for details on how this is implemented.
    */
  val NODE_NETWORK_LIMITED: ServiceIdentifier = ServiceIdentifier(1 << 10)

  private case class ServiceIdentifierImpl(num: UInt64)
      extends ServiceIdentifier

  def fromBytes(bytes: ByteVector): ServiceIdentifier =
    RawServiceIdentifierSerializer.read(bytes)

  def fromString(string: String): ServiceIdentifier = string match {
    case "NETWORK"         => NODE_NETWORK
    case "NETWORK_LIMITED" => NODE_NETWORK_LIMITED
    case "WITNESS"         => NODE_WITNESS
    case "BLOOM"           => NODE_BLOOM
    case "GETUTXO"         => NODE_GET_UTXO
    case "COMPACT_FILTERS" => NODE_COMPACT_FILTERS
    case "XTHIN"           => NODE_XTHIN
    case _: String =>
      throw new IllegalArgumentException(
        s""""$string" does not represent a ServiceIdentifier""")
  }

  def apply(num: BigInt): ServiceIdentifier = ServiceIdentifier(UInt64(num))

  def apply(uInt64: UInt64): ServiceIdentifier = ServiceIdentifierImpl(uInt64)
}
