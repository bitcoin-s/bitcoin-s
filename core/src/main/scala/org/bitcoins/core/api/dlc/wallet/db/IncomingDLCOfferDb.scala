package org.bitcoins.core.api.dlc.wallet.db

import org.bitcoins.core.protocol.tlv.{DLCOfferTLV, LnMessage, SendOfferTLV}
import org.bitcoins.crypto.Sha256Digest

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

  def toTLV: SendOfferTLV = {
    require(peer.nonEmpty)
    require(message.nonEmpty)
    SendOfferTLV(offer = offerTLV, message = message.get, peer = peer.get)
  }

  def toMessage: LnMessage[SendOfferTLV] = {
    LnMessage(this.toTLV)
  }

}
