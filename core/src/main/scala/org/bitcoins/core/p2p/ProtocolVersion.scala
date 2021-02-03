package org.bitcoins.core.p2p

import org.bitcoins.crypto.{Factory, NetworkElement}
import scodec.bits._

/** The peer to peer network has versions to allow for new operations
  * Here are the currently protocol versions in the network
  * @see [[https://bitcoin.org/en/developer-reference#protocol-versions]]
  */
sealed trait ProtocolVersion extends NetworkElement

object ProtocolVersion extends Factory[ProtocolVersion] {

  /** The default protocol version */
  val default: ProtocolVersion = ProtocolVersion70013

  /** The Bitcoin-S node useragent */
  val userAgent = "/bitcoins-spv-node/0.0.1"

  val versions: Seq[ProtocolVersion] = List(
    ProtocolVersion106,
    ProtocolVersion209,
    ProtocolVersion311,
    ProtocolVersion31402,
    ProtocolVersion31800,
    ProtocolVersion60000,
    ProtocolVersion60001,
    ProtocolVersion60002,
    ProtocolVersion70001,
    ProtocolVersion70002,
    ProtocolVersion70012,
    ProtocolVersion70013,
    ProtocolVersion70014,
    ProtocolVersion70015,
    ProtocolVersion70016
  )

  def fromBytes(bytes: ByteVector): ProtocolVersion = {
    //TODO: Should we default to the latest protocol version if the bytes don't match???
    versions.find(v => v.bytes == bytes).getOrElse(versions.last)
  }
}

/** Added receive IP address fields to version message.
  * Bitcoin Core 0.1.6 (Oct 2009)
  */
case object ProtocolVersion106 extends ProtocolVersion {
  override val bytes: ByteVector = hex"6a000000"
}

/** Added checksum field to message headers.
  * Bitcoin Core 0.2.9 (May 2010)
  */
case object ProtocolVersion209 extends ProtocolVersion {
  override val bytes: ByteVector = hex"d1000000"
}

/** Added alert message
  * Bitcion Core 0.3.11 (Aug 2010)
  */
case object ProtocolVersion311 extends ProtocolVersion {
  override val bytes: ByteVector = hex"37010000"
}

/** Added time field to addr message.
  * Bitcoin Core 0.3.15 (Oct 2010)
  */
case object ProtocolVersion31402 extends ProtocolVersion {
  override val bytes: ByteVector = hex"aa7a0000"
}

/** Added getheaders message and headers message.
  * Bitcoin Core 0.3.18 (Dec 2010)
  */
case object ProtocolVersion31800 extends ProtocolVersion {
  override val bytes: ByteVector = hex"387c0000"
}

/** BIP14: Separated protocol version from Bitcoin Core version
  * Bitcoin Core 0.6.0 (Mar 2012)
  */
case object ProtocolVersion60000 extends ProtocolVersion {
  override val bytes: ByteVector = hex"60ea0000"
}

/** BIP31: Added nonce field to ping message, Added pong message
  * Bitcoin Core 0.6.1 (May 2012)
  */
case object ProtocolVersion60001 extends ProtocolVersion {
  override val bytes: ByteVector = hex"61ea0000"
}

/** BIP35: Added mempool message.
  * • Extended getdata message to allow download of memory pool transactions
  * Bitcoin Core 0.7.0 (Sep 2012)
  */
case object ProtocolVersion60002 extends ProtocolVersion {
  override val bytes: ByteVector = hex"62ea0000"
}

/** Added notfound message.
  * BIP37:
  * • Added filterload message.
  * • Added filteradd message.
  * • Added filterclear message.
  * • Added merkleblock message.
  * • Added relay field to version message
  * • Added MSG_FILTERED_BLOCK inventory type to getdata message.
  * Bitcoin Core 0.8.0 (Feb 2013)
  */
case object ProtocolVersion70001 extends ProtocolVersion {
  override val bytes: ByteVector = hex"71110100"
}

/** Send multiple inv messages in response to a mempool message if necessary
  * BIP61: Add reject message
  * Bitcoin Core 0.9.0 (Mar 2014)
  */
case object ProtocolVersion70002 extends ProtocolVersion {
  override val bytes: ByteVector = hex"72110100"
}

/** BIP130: Add sendheaders message
  * Bitcoin Core 0.12.0
  */
case object ProtocolVersion70012 extends ProtocolVersion {
  override val bytes: ByteVector = hex"7c110100"
}

/** Added feefilter message.
  * Removed alert message system. See Alert System Retirement
  * Bitcoin Core 0.13.0 (August 2016)
  */
case object ProtocolVersion70013 extends ProtocolVersion {
  override val bytes: ByteVector = hex"7d110100"
}

/** BIP152
  * Added sendcmpct, cmpctblock, getblocktxn, blocktxn messages
  * Added MSG_CMPCT_BLOCK inventory type to getdata message.
  * Bitcoin Core 0.13.0 (August 2016)
  */
case object ProtocolVersion70014 extends ProtocolVersion {
  override val bytes: ByteVector = hex"7e110100"
}

/** New banning behavior for invalid compact blocks #9026 in v0.14.0, Backported to v0.13.2 in #9048.
  * Bitcoin Core 0.13.2 (January 2017)
  */
case object ProtocolVersion70015 extends ProtocolVersion {
  override val bytes: ByteVector = hex"7f110100"
}

/** Indicates that a node prefers to relay transactions via wtxid, rather than txid.
  * Protocol version 70016 as described by BIP 339.
  * Bitcoin Core 0.21.0 (December 2020)
  */
case object ProtocolVersion70016 extends ProtocolVersion {
  override val bytes: ByteVector = hex"80110100"
}
