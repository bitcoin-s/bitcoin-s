package org.bitcoins.core.protocol.ln.routing

import java.math.BigInteger

import org.bitcoins.core.crypto.ECPublicKey
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.NetworkElement
import org.bitcoins.core.protocol.ln.ShortChannelId
import org.bitcoins.core.protocol.ln.currency.MilliSatoshis
import org.bitcoins.core.protocol.ln.fee.{ FeeBaseMSat, FeeProportionalMillionths }
import org.bitcoins.core.util.BitcoinSUtil
import scodec.bits.ByteVector

/**
 * Indicates a node to route through with specific options on the Lightning Network
 * For more details on these settings please see
 * [[https://github.com/lightningnetwork/lightning-rfc/blob/master/02-peer-protocol.md#cltv_expiry_delta-selection BOLT2]]
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

  def fromBytes(bytes: ByteVector): LnRoute = {

    val pubKey = ECPublicKey.fromBytes(bytes.take(33))

    val shortChannelId = ShortChannelId.fromBytes(bytes.slice(33, 41))

    val feeBaseU32 = UInt32.fromBytes(bytes.slice(41, 45))
    val feeBase = feeBaseU32.toLong

    val feeBaseMSat = FeeBaseMSat(MilliSatoshis(feeBase))

    val u32 = UInt32.fromBytes(bytes.slice(45, 49))

    val feeProportionalMillionths = FeeProportionalMillionths(u32)

    val cltvExpiryDelta = new BigInteger(bytes.slice(49, 51).toArray).shortValueExact()

    LnRoute(pubKey, shortChannelId, feeBaseMSat, feeProportionalMillionths, cltvExpiryDelta)

  }
}