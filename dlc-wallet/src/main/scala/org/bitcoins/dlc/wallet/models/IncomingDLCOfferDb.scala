package org.bitcoins.dlc.wallet.models

import org.bitcoins.core.protocol.tlv.DLCOfferTLV
import org.bitcoins.crypto.{CryptoUtil, Sha256Digest}

import java.time.Instant

case class IncomingDLCOfferDb(
    hash: Sha256Digest,
    receivedAt: Instant,
    peer: Option[String],
    message: Option[String],
    offerTLV: DLCOfferTLV) {
  require(peer.forall(_.length <= 1024),
          "peer length must not exceed 1024 characters")
  require(message.forall(_.length <= 1024),
          "message length must not exceed 1024 characters")
}

object IncomingDLCOfferDbHelper {

  def fromTLV(
      offerTLV: DLCOfferTLV,
      peer: Option[String] = None,
      message: Option[String] = None,
      receivedAt: Instant = Instant.now()): IncomingDLCOfferDb = {
    IncomingDLCOfferDb(
      hash = CryptoUtil.sha256(offerTLV.bytes),
      receivedAt = receivedAt,
      peer = peer,
      message = message,
      offerTLV = offerTLV
    )
  }
}
