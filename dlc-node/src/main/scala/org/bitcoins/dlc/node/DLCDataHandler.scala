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
    case lnMessage: LnMessage[TLV] =>
      log.info(s"Received LnMessage ${lnMessage.typeName}")
      lnMessage.tlv match {
        case msg @ (_: UnknownTLV | _: DLCOracleTLV | _: DLCSetupPieceTLV) =>
          log.error(s"Received unhandled message $msg")
        case _: InitTLV =>
          () // todo init logic
        case error: ErrorTLV =>
          log.error(error.toString)
        case ping: PingTLV =>
          val pong = PongTLV.forIgnored(ping.ignored)
          connectionHandler ! LnMessage(pong)
        case pong: PongTLV =>
          log.debug(s"Received pong message $pong")
        case dlcOffer: DLCOfferTLV =>
          val _ = for {
            accept <- dlcWalletApi.acceptDLCOffer(dlcOffer)
            _ = connectionHandler ! accept.toMessage
          } yield ()
        case dlcAccept: DLCAcceptTLV =>
          val _ = for {
            sign <- dlcWalletApi.signDLC(dlcAccept)
            _ = connectionHandler ! sign.toMessage
          } yield ()
        case dlcSign: DLCSignTLV =>
          val _ = for {
            _ <- dlcWalletApi.addDLCSigs(dlcSign)
            _ <- dlcWalletApi.broadcastDLCFundingTx(dlcSign.contractId)
          } yield ()
      }
    case DLCConnectionHandler.WriteFailed(_) =>
      log.error("Write failed")
    case Terminated(actor) if actor == connectionHandler =>
      context.stop(self)
  }
}

object DLCDataHandler {

  type Factory = (DLCWalletApi, ActorContext, ActorRef) => ActorRef

  sealed trait Command
  case class Received(tlv: TLV) extends Command
  case class Send(tlv: TLV) extends Command

  def defaultFactory(
      dlcWalletApi: DLCWalletApi,
      context: ActorContext,
      connectionHandler: ActorRef): ActorRef = {
    context.actorOf(props(dlcWalletApi, connectionHandler))
  }

  def props(dlcWalletApi: DLCWalletApi, connectionHandler: ActorRef): Props =
    Props(new DLCDataHandler(dlcWalletApi, connectionHandler))
}
