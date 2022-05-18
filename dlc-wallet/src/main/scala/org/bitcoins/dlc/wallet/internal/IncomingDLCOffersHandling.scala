package org.bitcoins.dlc.wallet.internal

import org.bitcoins.core.api.dlc.wallet.db.{DLCContactDb, IncomingDLCOfferDb}
import org.bitcoins.core.protocol.tlv.DLCOfferTLV
import org.bitcoins.crypto.Sha256Digest
import org.bitcoins.dlc.wallet.DLCWallet
import org.bitcoins.dlc.wallet.models.IncomingDLCOfferDbHelper

import java.net.InetSocketAddress
import scala.concurrent.Future

trait IncomingDLCOffersHandling { self: DLCWallet =>

  def registerIncomingDLCOffer(
      offerTLV: DLCOfferTLV,
      peer: Option[String],
      message: Option[String]): Future[Sha256Digest] = {
    val dbo = IncomingDLCOfferDbHelper.fromTLV(offerTLV = offerTLV,
                                               peer = peer,
                                               message = message)
    for {
      added <- dlcWalletDAOs.incomingDLCOfferDAO.create(dbo)
      _ <- dlcConfig.walletCallbacks.executeOnDLCOfferAdd(logger, added)
    } yield dbo.hash
  }

  def rejectIncomingDLCOffer(offerHash: Sha256Digest): Future[Unit] = {
    for {
      _ <- dlcWalletDAOs.incomingDLCOfferDAO.delete(offerHash)
      _ <- dlcConfig.walletCallbacks.executeOnDLCOfferRemove(logger, offerHash)
    } yield ()
  }

  def listIncomingDLCOffers(): Future[Vector[IncomingDLCOfferDb]] = {
    dlcWalletDAOs.incomingDLCOfferDAO.findAll()
  }

  def findIncomingDLCOffer(
      offerHash: Sha256Digest): Future[Option[IncomingDLCOfferDb]] = {
    dlcWalletDAOs.incomingDLCOfferDAO.find(offerHash)
  }

  override def listDLCContacts(): Future[Vector[DLCContactDb]] =
    contactDAO.findAll()

  override def addDLCContact(contact: DLCContactDb): Future[Unit] = {
    for {
      _ <- contactDAO.create(contact)
    } yield ()
  }

  override def removeDLCContact(address: InetSocketAddress): Future[Unit] = {
    for {
      _ <- contactDAO.delete(address)
    } yield ()
  }

  override def findDLCContacts(alias: String): Future[Vector[DLCContactDb]] =
    contactDAO.findByAlias(alias)

  override def addDLCContactMapping(
      dlcId: Sha256Digest,
      contcatId: InetSocketAddress): Future[Unit] = {
    dlcContactMappingDAO
      .create(dlcId, contcatId)
      .map(_ => ())
  }

  override def removeDLCContactMapping(dlcId: Sha256Digest): Future[Unit] = {
    dlcContactMappingDAO.delete(dlcId)
  }
}
