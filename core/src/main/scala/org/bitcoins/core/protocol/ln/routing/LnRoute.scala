package org.bitcoins.core.protocol.ln.routing

import java.math.BigInteger

import org.bitcoins.core.crypto.ECPublicKey
import org.bitcoins.core.number.{ Int64, UInt32, UInt5 }
import org.bitcoins.core.protocol.NetworkElement
import org.bitcoins.core.protocol.ln.ShortChannelId
import org.bitcoins.core.protocol.ln.currency.{ MilliSatoshis, PicoBitcoins }
import org.bitcoins.core.protocol.ln.fee.{ FeeBaseMSat, FeeProportionalMillionths }
import org.bitcoins.core.util.BitcoinSUtil
import org.slf4j.LoggerFactory
import scodec.bits.ByteVector

/**
 * Indicates a node to route through with specific options on the Lightning Network
 * For more details on these settings please see
 * [[https://github.com/lightningnetwork/lightning-rfc/blob/master/02-peer-protocol.md#cltv_expiry_delta-selection]]
 */
case class LnRoute(
  pubkey: ECPublicKey,
  shortChannelID: ShortChannelId,
  feeBaseMsat: FeeBaseMSat,
  feePropMilli: FeeProportionalMillionths,
  cltvExpiryDelta: Short) extends NetworkElement {

  require(pubkey.isCompressed, s"Can only use a compressed public key in routing")

  override def bytes: ByteVector = {

    val cltvExpiryDeltaHex = BitcoinSUtil.encodeHex(cltvExpiryDelta)

    pubkey.bytes ++
      shortChannelID.bytes ++
      feeBaseMsat.bytes ++
      feePropMilli.bytes ++
      ByteVector.fromValidHex(cltvExpiryDeltaHex)
  }
}

object LnRoute {
  private val logger = LoggerFactory.getLogger(this.getClass.getSimpleName)
  def fromBytes(bytes: ByteVector): LnRoute = {

    val pubKey = ECPublicKey.fromBytes(bytes.take(33))

    val shortChannelId = ShortChannelId.fromBytes(bytes.slice(33, 41))

    //type hacking here, fee base is a uint32, but PicoBitcoins needs a i64
    //if we parse directly to an i64 we risk having the largest big endian byte
    //be signed -- which will give us a negative number possibly
    val feeBaseU32 = UInt32.fromBytes(bytes.slice(41, 45))
    val feeBase = feeBaseU32.toLong

    val feeBaseMSat = FeeBaseMSat(MilliSatoshis(feeBase))

    val u32 = UInt32.fromBytes(bytes.slice(45, 49))

    val feeProportionalMillionths = FeeProportionalMillionths(u32)

    val cltvExpiryDelta = new BigInteger(bytes.slice(49, 51).toArray).shortValueExact()

    LnRoute(pubKey, shortChannelId, feeBaseMSat, feeProportionalMillionths, cltvExpiryDelta)

  }
}