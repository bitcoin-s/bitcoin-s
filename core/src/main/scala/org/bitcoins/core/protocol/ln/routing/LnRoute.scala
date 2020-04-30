package org.bitcoins.core.protocol.ln.routing

import java.math.BigInteger

import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.ln.ShortChannelId
import org.bitcoins.core.protocol.ln.currency.MilliSatoshis
import org.bitcoins.core.protocol.ln.fee.{
  FeeBaseMSat,
  FeeProportionalMillionths
}
import org.bitcoins.crypto.{BytesUtil, ECPublicKey, NetworkElement}
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
    cltvExpiryDelta: Short)
    extends NetworkElement {

  require(pubkey.isCompressed,
          s"Can only use a compressed public key in routing")

  override def bytes: ByteVector = {

    val cltvExpiryDeltaHex = BytesUtil.encodeHex(cltvExpiryDelta)

    pubkey.bytes ++
      shortChannelID.bytes ++
      feeBaseMsat.bytes ++
      feePropMilli.bytes ++
      ByteVector.fromValidHex(cltvExpiryDeltaHex)
  }
}

object LnRoute {

  def fromBytes(bytes: ByteVector): LnRoute = {
    val PUBKEY_LEN = 33
    val SHORT_CHANNEL_ID_LEN = 8
    val FEE_BASE_U32_LEN = 4
    val FEE_PROPORTIONAL_LEN = 4
    val CLTV_EXPIRTY_DELTA_LEN = 2

    val TOTAL_LEN = PUBKEY_LEN +
      SHORT_CHANNEL_ID_LEN +
      FEE_BASE_U32_LEN +
      FEE_PROPORTIONAL_LEN +
      CLTV_EXPIRTY_DELTA_LEN

    require(
      bytes.length >= TOTAL_LEN,
      s"ByteVector must at least of length $TOTAL_LEN, got ${bytes.length}")

    val (pubKeyBytes, rest0) = bytes.splitAt(PUBKEY_LEN)
    val pubKey = ECPublicKey.fromBytes(pubKeyBytes)

    val (shortChannelIdBytes, rest1) = rest0.splitAt(SHORT_CHANNEL_ID_LEN)

    val shortChannelId = ShortChannelId.fromBytes(shortChannelIdBytes)

    val (feeBaseU32Bytes, rest2) = rest1.splitAt(FEE_BASE_U32_LEN)

    val feeBaseU32 = UInt32.fromBytes(feeBaseU32Bytes)
    val feeBase = feeBaseU32.toLong
    val feeBaseMSat = FeeBaseMSat(MilliSatoshis(feeBase))

    val (u32Bytes, rest3) = rest2.splitAt(FEE_PROPORTIONAL_LEN)

    val u32 = UInt32.fromBytes(u32Bytes)
    val feeProportionalMillionths = FeeProportionalMillionths(u32)

    val (cltvExpiryDeltaBytes, _) = rest3.splitAt(CLTV_EXPIRTY_DELTA_LEN)

    val cltvExpiryDelta = new BigInteger(cltvExpiryDeltaBytes.toArray).shortValueExact

    LnRoute(pubKey,
            shortChannelId,
            feeBaseMSat,
            feeProportionalMillionths,
            cltvExpiryDelta)
  }
}
