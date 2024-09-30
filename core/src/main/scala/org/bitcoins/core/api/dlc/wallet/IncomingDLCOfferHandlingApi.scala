package org.bitcoins.core.api.dlc.wallet

import org.bitcoins.core.api.dlc.wallet.db.{DLCContactDb, IncomingDLCOfferDb}
import org.bitcoins.core.protocol.tlv.DLCOfferTLV
import org.bitcoins.crypto.Sha256Digest

import java.net.InetSocketAddress
import scala.concurrent.Future

trait IncomingDLCOfferHandlingApi {
  def registerIncomingDLCOffer(
      offerTLV: DLCOfferTLV,
      peer: Option[String],
      message: Option[String]): Future[Sha256Digest]

  def listIncomingDLCOffers(): Future[Vector[IncomingDLCOfferDb]]

  def rejectIncomingDLCOffer(offerHash: Sha256Digest): Future[Unit]

  def findIncomingDLCOffer(
      offerHash: Sha256Digest): Future[Option[IncomingDLCOfferDb]]

  def listDLCContacts(): Future[Vector[DLCContactDb]]

  def addDLCContact(contact: DLCContactDb): Future[Unit]

  def removeDLCContact(address: InetSocketAddress): Future[Unit]

  def findDLCContacts(alias: String): Future[Vector[DLCContactDb]]

  def addDLCContactMapping(
      dlcId: Sha256Digest,
      contactId: InetSocketAddress): Future[Unit]

  def removeDLCContactMapping(dlcId: Sha256Digest): Future[Unit]
}
