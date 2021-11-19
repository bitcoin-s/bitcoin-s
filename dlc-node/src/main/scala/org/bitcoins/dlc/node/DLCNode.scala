package org.bitcoins.dlc.node

import akka.actor.{ActorRef, ActorSystem}
import grizzled.slf4j.Logging
import org.bitcoins.core.api.dlc.node.DLCNodeApi
import org.bitcoins.core.api.dlc.wallet.DLCWalletApi
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.dlc.node.config._
import org.bitcoins.dlc.node.peer.Peer

import java.net.InetSocketAddress
import scala.concurrent._

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
        config.torParams
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

  private[node] def connectAndSendToPeer(
      peerAddress: InetSocketAddress,
      message: LnMessage[TLV]): Future[Unit] = {
    val peer =
      Peer(socket = peerAddress, socks5ProxyParams = config.socks5ProxyParams)

    val handlerP = Promise[ActorRef]()

    for {
      _ <- DLCClient.connect(peer, wallet, Some(handlerP))
      handler <- handlerP.future
    } yield handler ! message
  }

  def acceptDLCOffer(
      peerAddress: InetSocketAddress,
      dlcOffer: LnMessage[DLCOfferTLV]): Future[Unit] = {
    connectAndSendToPeer(peerAddress, dlcOffer)
  }
}
