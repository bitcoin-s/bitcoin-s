package org.bitcoins.dlc.node

import akka.actor._
import akka.event.LoggingReceive
import org.bitcoins.core.api.dlc.wallet.DLCWalletApi
import org.bitcoins.core.protocol.tlv._

import scala.concurrent._

class DLCDataHandler(dlcWalletApi: DLCWalletApi, connectionHandler: ActorRef)
    extends Actor
    with ActorLogging {
  implicit val ec: ExecutionContextExecutor = context.system.dispatcher

  override def preStart(): Unit = {
    val _ = context.watch(connectionHandler)
  }

  override def receive: Receive = LoggingReceive {
    case DLCDataHandler.Send(lnMessage) =>
      send(lnMessage)
    case DLCDataHandler.Received(lnMessage) =>
      log.info(s"Received LnMessage ${lnMessage.typeName}")
      val f: Future[Unit] = handleTLVMessage(lnMessage)
      f.failed.foreach(err =>
        log.error(err, s"Failed to process lnMessage=${lnMessage} "))
    case DLCConnectionHandler.WriteFailed(_) =>
      log.error("Write failed")
    case Terminated(actor) if actor == connectionHandler =>
      context.stop(self)
  }

  private def handleTLVMessage(lnMessage: LnMessage[TLV]): Future[Unit] = {
    lnMessage.tlv match {
      case msg @ (_: UnknownTLV | _: DLCOracleTLV | _: DLCSetupPieceTLV) =>
        log.error(s"Received unhandled message $msg")
        Future.unit
      case _: InitTLV =>
        Future.unit // todo init logic
      case error: ErrorTLV =>
        log.error(error.toString)
        Future.unit //is this right?
      case ping: PingTLV =>
        val pong = PongTLV.forIgnored(ping.ignored)
        send(LnMessage(pong))
        Future.unit
      case pong: PongTLV =>
        log.debug(s"Received pong message $pong")
        Future.unit
      case dlcOffer: DLCOfferTLV =>
        for {
          _ <- dlcWalletApi.registerIncomingDLCOffer(dlcOffer, None, None)
        } yield ()
      case dlcOfferMessage: SendOfferTLV =>
        for {
          _ <- dlcWalletApi.registerIncomingDLCOffer(
            offerTLV = dlcOfferMessage.offer,
            peer = Some(dlcOfferMessage.peer),
            message = Some(dlcOfferMessage.message))
        } yield ()
      case dlcAccept: DLCAcceptTLV =>
        val f = for {
          sign <- dlcWalletApi.signDLC(dlcAccept)
        } yield {
          send(sign.toMessage)
        }
        f
      case dlcSign: DLCSignTLV =>
        val f = for {
          _ <- dlcWalletApi.addDLCSigs(dlcSign)
          _ <- dlcWalletApi.broadcastDLCFundingTx(dlcSign.contractId)
        } yield ()
        f
    }
  }

  private def send(lnMessage: LnMessage[TLV]): Unit = {
    connectionHandler ! lnMessage
  }
}

object DLCDataHandler {

  type Factory = (DLCWalletApi, ActorContext, ActorRef) => ActorRef

  sealed trait Command
  case class Received(message: LnMessage[TLV]) extends Command
  case class Send(message: LnMessage[TLV]) extends Command

  def defaultFactory(
      dlcWalletApi: DLCWalletApi,
      context: ActorContext,
      connectionHandler: ActorRef): ActorRef = {
    context.actorOf(props(dlcWalletApi, connectionHandler))
  }

  def props(dlcWalletApi: DLCWalletApi, connectionHandler: ActorRef): Props =
    Props(new DLCDataHandler(dlcWalletApi, connectionHandler))
}
