package org.bitcoins.dlc.node

import akka.actor.{
  Actor,
  ActorContext,
  ActorLogging,
  ActorRef,
  Props,
  Terminated
}
import akka.event.LoggingReceive
import org.bitcoins.core.protocol.tlv.TLV

class DLCDataHandler(connectionHandler: ActorRef)
    extends Actor
    with ActorLogging {

  override def preStart(): Unit = {
    val _ = context.watch(connectionHandler)
  }

  override def receive: Receive = LoggingReceive {
    case tlv: TLV =>
      log.info(s"received TLV ${tlv.typeName}")
    case DLCConnectionHandler.WriteFailed(_) =>
      log.error("Write failed")
    case Terminated(actor) if actor == connectionHandler =>
      context.stop(self)
  }
}

object DLCDataHandler {

  type Factory = (ActorContext, ActorRef) => ActorRef

  sealed trait Command
  case class Received(tlv: TLV) extends Command
  case class Send(tlv: TLV) extends Command

  def defaultFactory(
      context: ActorContext,
      connectionHandler: ActorRef): ActorRef = {
    context.actorOf(props(connectionHandler))
  }

  def props(connectionHandler: ActorRef) = Props(
    new DLCDataHandler(connectionHandler))
}
