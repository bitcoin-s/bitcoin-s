package org.bitcoins.dlc.node

import akka.actor.{ActorRef, ActorSystem}
import grizzled.slf4j.Logging
import org.bitcoins.core.api.dlc.node.DLCNodeApi
import org.bitcoins.core.api.dlc.wallet.DLCWalletApi
import org.bitcoins.core.protocol.dlc.models.DLCMessage
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.core.protocol.{BigSizeUInt, BitcoinAddress}
import org.bitcoins.crypto.Sha256Digest
import org.bitcoins.dlc.node.config._
import org.bitcoins.dlc.node.peer.Peer
import scodec.bits.ByteVector

import java.net.InetSocketAddress
import scala.concurrent._
import scala.util.{Failure, Success}

case class DLCNode(wallet: DLCWalletApi)(implicit
    system: ActorSystem,
    val config: DLCNodeAppConfig)
    extends DLCNodeApi
    with Logging {

  implicit val ec: ExecutionContextExecutor = system.dispatcher

  private[node] lazy val serverBindF: Future[(InetSocketAddress, ActorRef)] = {
    logger.info(
      s"Binding server to ${config.listenAddress}, with tor hidden service: ${config.torParams.isDefined}")
    DLCServer
      .bind(
        wallet,
        config.listenAddress,
        config.torConf.targets,
        config.torParams,
        handleWrite = handleTLVSendSucceed,
        handleWriteError = handleTLVSendFailed
      )
      .map { case (addr, actor) =>
        hostAddressP.success(addr)
        (addr, actor)
      }
  }

  private val hostAddressP: Promise[InetSocketAddress] =
    Promise[InetSocketAddress]()

  def getHostAddress: Future[InetSocketAddress] = {
    config.externalIP match {
      case Some(address) => Future.successful(address)
      case None          => hostAddressP.future
    }
  }

  override def start(): Future[Unit] = {
    serverBindF.map(_ => ())
  }

  override def stop(): Future[Unit] = {
    serverBindF.map { case (_, actorRef) =>
      system.stop(actorRef)
    }
  }

  override def acceptDLCOffer(
      peerAddress: InetSocketAddress,
      dlcOffer: LnMessage[DLCOfferTLV],
      externalPayoutAddress: Option[BitcoinAddress],
      externalChangeAddress: Option[BitcoinAddress]): Future[
    DLCMessage.DLCAccept] = {
    val f = for {
      handler <- connectToPeer(peerAddress)
      accept <- wallet.acceptDLCOffer(dlcOffer.tlv,
                                      Some(peerAddress),
                                      externalPayoutAddress,
                                      externalChangeAddress)
    } yield {
      handler ! DLCDataHandler.Send(accept.toMessage)
      accept
    }

    f.failed.foreach(err =>
      config.callBacks.executeOnAcceptFailed(dlcOffer.tlv.tempContractId,
                                             err.getMessage))

    f
  }

  override def sendDLCOffer(
      peerAddress: InetSocketAddress,
      message: String,
      offerTLV: DLCOfferTLV): Future[Sha256Digest] = {
    val f = for {
      handler <- connectToPeer(peerAddress)
      localAddress <- getHostAddress
    } yield {
      val peer = NormalizedString(
        localAddress.getHostString + ":" + peerAddress.getPort)
      val msg = NormalizedString(message)
      val lnMessage = LnMessage(
        SendOfferTLV(peer = peer, message = msg, offer = offerTLV))
      handler ! DLCDataHandler.Send(lnMessage)
      offerTLV.tempContractId
    }

    f.failed.foreach { err =>
      logger.error(
        s"Failed to send offer.tempContractId=${offerTLV.tempContractId}",
        err)
      config.callBacks.executeOnOfferSendFailed(offerTLV.tempContractId,
                                                err.getMessage)
    }

    f
  }

  override def sendDLCOffer(
      peerAddress: InetSocketAddress,
      message: String,
      temporaryContractId: Sha256Digest): Future[Sha256Digest] = {
    for {
      dlcOpt <- wallet.findDLCByTemporaryContractId(temporaryContractId)
      dlc = dlcOpt.getOrElse(
        throw new IllegalArgumentException(
          s"Cannot find a DLC with temp contact ID $temporaryContractId"))
      offerOpt <- wallet.getDLCOffer(dlc.dlcId)
      offer = offerOpt.getOrElse(
        throw new IllegalArgumentException(
          s"Cannot find an offer with for DLC ID ${dlc.dlcId}"))
      res <- sendDLCOffer(peerAddress, message, offer.toTLV)
    } yield res
  }

  override def checkPeerConnection(
      peerAddress: InetSocketAddress): Future[Unit] = {
    for {
      handler <- connectToPeer(peerAddress)
    } yield {
      handler ! DLCConnectionHandler.CloseConnection
    }
  }

  private def handleTLVSendFailed(
      tlvType: BigSizeUInt,
      tlvId: ByteVector,
      error: Throwable): Future[Unit] = {
    logger.info("TLV send error ", error)
    tlvType match {
      case SendOfferTLV.tpe | DLCOfferTLV.tpe =>
        config.callBacks.executeOnOfferSendFailed(Sha256Digest.fromBytes(tlvId),
                                                  error.getMessage)
      case DLCAcceptTLV.tpe =>
        config.callBacks.executeOnAcceptFailed(Sha256Digest.fromBytes(tlvId),
                                               error.getMessage)
      case DLCSignTLV.tpe =>
        config.callBacks.executeOnSignFailed(Sha256Digest.fromBytes(tlvId),
                                             error.getMessage)
      case unknown =>
        val exn = new RuntimeException(
          s"Unknown tpe=$unknown inside of handleTLVSendFailed")
        Future.failed(exn)
    }
  }

  private def handleTLVSendSucceed(
      tlvType: BigSizeUInt,
      tlvId: ByteVector): Future[Unit] = {
    tlvType match {
      case SendOfferTLV.tpe | DLCOfferTLV.tpe =>
        config.callBacks.executeOnOfferSendSucceed(
          Sha256Digest.fromBytes(tlvId))
      case DLCAcceptTLV.tpe =>
        config.callBacks.executeOnAcceptSucceed(Sha256Digest.fromBytes(tlvId))
      case DLCSignTLV.tpe =>
        config.callBacks.executeOnSignSucceed(Sha256Digest.fromBytes(tlvId))
      case unknown =>
        val exn = new RuntimeException(
          s"Unknown tpe=$unknown inside of handleTLVSendSucceed")
        Future.failed(exn)
    }
  }

  private def connectToPeer(
      peerAddress: InetSocketAddress): Future[ActorRef] = {

    val peer =
      Peer(socket = peerAddress, socks5ProxyParams = config.socks5ProxyParams)

    val handlerP = Promise[ActorRef]()

    val f = for {
      _ <- config.callBacks.executeOnPeerConnectionInitiated(peerAddress)
      _ <- DLCClient.connect(peer,
                             wallet,
                             Some(handlerP),
                             handleWrite = handleTLVSendSucceed,
                             handleWriteError = handleTLVSendFailed)
      handler <- handlerP.future
    } yield handler

    f.onComplete {
      case Success(_) =>
        config.callBacks.executeOnPeerConnectionEstablished(peerAddress)
      case Failure(err) =>
        logger.error(s"Failed to establish connect to peer=$peerAddress", err)
        config.callBacks.executeOnPeerConnectionFailed(peerAddress)
    }

    f
  }
}
