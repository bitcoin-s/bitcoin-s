package org.bitcoins.core.protocol.ln.fee

import org.bitcoins.core.number.{ Int32, UInt32 }
import org.bitcoins.core.protocol.NetworkElement
import org.bitcoins.core.protocol.ln.{ LnCurrencyUnit, PicoBitcoins }
import scodec.bits.ByteVector

/**
 * Represents the fee we charge for forwarding an HTLC on the Lightning network
 * This is used in the ChannelUpdate and Routing information a [[org.bitcoins.core.protocol.ln.LnInvoice]]
 * [[https://github.com/lightningnetwork/lightning-rfc/blob/master/07-routing-gossip.md#the-channel_update-message]]
 * [[https://github.com/lightningnetwork/lightning-rfc/blob/master/11-payment-encoding.md#on-mainnet-with-fallback-address-1rustyrx2oai4eyydpqgwvel62bbgqn9t-with-extra-routing-info-to-go-via-nodes-029e03a901b85534ff1e92c43c74431f7ce72046060fcf7a95c37e148f78c77255-then-039e03a901b85534ff1e92c43c74431f7ce72046060fcf7a95c37e148f78c77255]]
 */
case class FeeBaseMSat(msat: PicoBitcoins) extends NetworkElement {
  require(msat.toLong <= UInt32.max.toLong, s"Value too large for FeeBaseMSat ${msat}")

  override def bytes: ByteVector = {
    //note that the feebase msat is only 4 bytes
    UInt32(msat.toLong).bytes
  }
}

/**
 * Represents the proportion of a satoshi we charge for forwarding an HTLC
 * through our channel. I.e. if the forwarded payment is larger, this fee will be larger
 * see BOLT7
 * [[https://github.com/lightningnetwork/lightning-rfc/blob/master/07-routing-gossip.md#the-channel_update-message]]
 * @param u32
 */
case class FeeProportionalMillionths(u32: UInt32) extends NetworkElement {
  override def bytes: ByteVector = u32.bytes
}
