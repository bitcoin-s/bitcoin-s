package org.bitcoins.dlc.wallet.internal

import org.bitcoins.core.api.dlc.wallet.db.IncomingDLCOfferDb
import org.bitcoins.core.protocol.tlv.DLCOfferTLV
import org.bitcoins.crypto.Sha256Digest
import org.bitcoins.dlc.wallet.DLCWallet
import org.bitcoins.dlc.wallet.models.IncomingDLCOfferDbHelper

import scala.concurrent.Future

trait IncomingDLCOffersHandling { self: DLCWallet =>

  def registerIncomingDLCOffer(
      offerTLV: DLCOfferTLV,
      peer: Option[String],
      message: Option[String]): Future[Sha256Digest] = {
    val dbo = IncomingDLCOfferDbHelper.fromTLV(offerTLV = offerTLV,
                                               peer = peer,
                                               message = message)
    dlcWalletDAOs.incomingDLCOfferDAO.create(dbo).map(_.hash)
  }

  def rejectIncomingDLCOffer(offerHash: Sha256Digest): Future[Unit] = {
    dlcWalletDAOs.incomingDLCOfferDAO.delete(offerHash).map(_ => ())
  }

  def listIncomingDLCOffers(): Future[Vector[IncomingDLCOfferDb]] = {
    dlcWalletDAOs.incomingDLCOfferDAO.findAll()
  }
}
