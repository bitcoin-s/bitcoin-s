package org.bitcoins.dlc.wallet.models

import org.bitcoins.core.api.dlc.wallet.db.IncomingDLCOfferDb
import org.bitcoins.core.protocol.tlv.DLCOfferTLV

import java.time.Instant

object IncomingDLCOfferDbHelper {

  def fromTLV(
      offerTLV: DLCOfferTLV,
      peer: Option[String] = None,
      message: Option[String] = None,
      receivedAt: Instant = Instant.now()): IncomingDLCOfferDb = {
    IncomingDLCOfferDb(
      hash = offerTLV.tempContractId,
      receivedAt = receivedAt,
      peer = peer,
      message = message,
      offerTLV = offerTLV
    )
  }
}
