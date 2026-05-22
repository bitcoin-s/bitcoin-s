package org.bitcoins.testkit.dlc

import org.bitcoins.core.api.dlc.node.DLCNodeApi
import org.bitcoins.core.api.dlc.wallet.{
  DLCWalletApi,
  IncomingDLCOfferHandlingApi
}
import org.bitcoins.core.api.dlc.wallet.db.{DLCContactDb, IncomingDLCOfferDb}
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.dlc.models.DLCMessage
import org.bitcoins.core.protocol.tlv.{DLCOfferTLV, LnMessage}
import org.bitcoins.crypto.CryptoUtil
import org.bitcoins.crypto.Sha256Digest
import org.bitcoins.testkit.wallet.DLCWalletUtil

import java.lang.reflect.{InvocationHandler, Method, Proxy}
import java.net.InetSocketAddress
import java.time.Instant
import scala.concurrent.Future

object MockDLCNodeApi {

  val hostAddress: InetSocketAddress = new InetSocketAddress("localhost", 2862)
  val sampleOfferMessageHex: String =
    "a71a0006226e46111a0b59caaf126043eb5bbf28c34f3a5e332a1fc7b2b73cf188910ffdd82efd010b000000000bebc200fda71039021331373539373000000000000000000531373539373100000000000bebc2053137353939000000000000000000fda712fdd824fd03289ee342f5c89cb97ef9291d7ebf6ebbf3d79f94453f8ff53fa6e00437092e1722c9f0d8f970f1755f09b86ecf2f9b49fbcd7cf8fa80fbeee81ac6f51eaf49dc4f95c24f758754ef6b8ca8c7ad51e86f6fd6be1cf9fb2fa26a5301fdd822fd0326f10001443f4f5cea3039d4a9504d32f0fa67e6fdbf7643c886680f3d445dc4129f8aa7459ed5f4ce97543c4f1f68511dd90fbe7ce8db70339dc7fc91fecf9a4ca824f9ee439e5ccf84f2e345609ee2ec18c4f95f40c8d5f4f0ee7dda8f0e3f8dfcba9f8dc0f7b74f2ca7a6d577d286073d853f9efe26dff0260f9a4dd2dfd70c5ac20a6547d2cf35de5f79d8f8f8d18921a4ffec6f6876c1f145f1dfff93a5251c5ff857f3d5811dce5bd9cf686220f3e93f8e553790908884504ca5820f781dd20a7f9ee2bc823322d97d3eb83067cd81ad16d604a7ebf7f53fe8c331bb6900fdd80a11000200074254432f55534400000000000f313735393730206c657373207468616e20313735393939000000"
  val offerLnMessageHex: String = LnMessage(
    DLCWalletUtil.sampleDLCOffer.toTLV).hex
  def fresh(): DLCNodeApi = new MockDLCNodeApiImpl

  private final class MockDLCNodeApiImpl extends DLCNodeApi {

    @volatile private var offers: Vector[IncomingDLCOfferDb] = Vector.empty
    @volatile private var contacts: Vector[DLCContactDb] = Vector.empty
    @volatile private var mappings: Map[Sha256Digest, InetSocketAddress] =
      Map.empty

    private val incomingOfferHandlingMock: IncomingDLCOfferHandlingApi =
      new IncomingDLCOfferHandlingApi {
        override def registerIncomingDLCOffer(
            offerTLV: DLCOfferTLV,
            peer: Option[String],
            message: Option[String]): Future[Sha256Digest] = synchronized {
          val hash = CryptoUtil.sha256(offerTLV.bytes)
          val db = IncomingDLCOfferDb(
            hash = hash,
            receivedAt = Instant.now(),
            peer = peer,
            message = message,
            offerTLV = offerTLV
          )
          offers = offers :+ db
          Future.successful(hash)
        }

        override def listIncomingDLCOffers()
            : Future[Vector[IncomingDLCOfferDb]] =
          synchronized {
            Future.successful(offers)
          }

        override def rejectIncomingDLCOffer(
            offerHash: Sha256Digest): Future[Unit] =
          synchronized {
            offers = offers.filterNot(_.hash == offerHash)
            Future.unit
          }

        override def findIncomingDLCOffer(
            offerHash: Sha256Digest): Future[Option[IncomingDLCOfferDb]] =
          synchronized {
            Future.successful(offers.find(_.hash == offerHash))
          }

        override def listDLCContacts(): Future[Vector[DLCContactDb]] =
          synchronized {
            Future.successful(contacts)
          }

        override def addDLCContact(contact: DLCContactDb): Future[Unit] =
          synchronized {
            contacts =
              contacts.filterNot(_.address == contact.address) :+ contact
            Future.unit
          }

        override def removeDLCContact(
            address: InetSocketAddress): Future[Unit] =
          synchronized {
            contacts = contacts.filterNot(_.address == address)
            Future.unit
          }

        override def findDLCContacts(
            alias: String): Future[Vector[DLCContactDb]] =
          synchronized {
            Future.successful(contacts.filter(_.alias == alias))
          }

        override def addDLCContactMapping(
            dlcId: Sha256Digest,
            contactId: InetSocketAddress): Future[Unit] = synchronized {
          mappings = mappings + (dlcId -> contactId)
          Future.unit
        }

        override def removeDLCContactMapping(
            dlcId: Sha256Digest): Future[Unit] =
          synchronized {
            mappings = mappings - dlcId
            Future.unit
          }
      }

    private val walletHandler: InvocationHandler = new InvocationHandler {
      override def invoke(
          proxy: Any,
          method: Method,
          args: Array[Object]): AnyRef = {
        method.getName match {
          case "incomingOfferHandling" =>
            incomingOfferHandlingMock.asInstanceOf[AnyRef]
          case "toString" => "MockDLCWalletApiProxy"
          case _ =>
            throw new UnsupportedOperationException(
              s"${method.getName} is not implemented in MockDLCNodeApi wallet proxy")
        }
      }
    }

    override lazy val wallet: DLCWalletApi =
      Proxy
        .newProxyInstance(
          classOf[DLCWalletApi].getClassLoader,
          Array(classOf[DLCWalletApi]),
          walletHandler
        )
        .asInstanceOf[DLCWalletApi]

    override def acceptDLCOffer(
        peerAddress: InetSocketAddress,
        dlcOffer: LnMessage[DLCOfferTLV],
        externalPayoutAddress: Option[BitcoinAddress],
        externalChangeAddress: Option[BitcoinAddress])
        : Future[DLCMessage.DLCAccept] =
      Future.failed(
        new UnsupportedOperationException(
          "acceptDLCOffer is not implemented in MockDLCNodeApi"))

    override def sendDLCOffer(
        peerAddress: InetSocketAddress,
        message: String,
        offerTLV: DLCOfferTLV): Future[Sha256Digest] =
      Future.successful(CryptoUtil.sha256(offerTLV.bytes))

    override def sendDLCOffer(
        peerAddress: InetSocketAddress,
        message: String,
        tempContractId: Sha256Digest): Future[Sha256Digest] =
      Future.successful(tempContractId)

    override def checkPeerConnection(
        peerAddress: InetSocketAddress): Future[Unit] =
      Future.unit

    override def getHostAddress: Future[InetSocketAddress] =
      Future.successful(hostAddress)

    override def start(): Future[Unit] = Future.unit

    override def stop(): Future[Unit] = Future.unit
  }
}
