package org.bitcoins.node.networking

import akka.actor.{Actor, ActorContext, ActorRef, Props}
import akka.event.LoggingReceive
import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.node.NetworkMessage
import org.bitcoins.node.constant.Constants
import org.bitcoins.node.messages.data.{GetDataMessage, Inventory}
import org.bitcoins.node.messages.{BlockMessage, TypeIdentifier}

/**
  * Created by chris on 7/10/16.
  */
sealed abstract class BlockActor extends Actor with BitcoinSLogger {

  def peerMsgHandler: ActorRef

  def receive: Receive = LoggingReceive {
    case hash: DoubleSha256Digest =>
      val inv = Inventory(TypeIdentifier.MsgBlock, hash)
      val getDataMessage = GetDataMessage(inv)
      val networkMessage =
        NetworkMessage(Constants.networkParameters, getDataMessage)
      peerMsgHandler ! networkMessage
      context.become(awaitBlockMsg)
    case blockHeader: BlockHeader =>
      self.forward(blockHeader.hash)
  }

  def awaitBlockMsg: Receive = LoggingReceive {
    case blockMsg: BlockMessage =>
      context.parent ! blockMsg
      context.stop(self)
  }
}

object BlockActor {
  private case class BlockActorImpl(
      peerMsgHandler: ActorRef
  ) extends BlockActor

  def props(peerMsgHandler: ActorRef): Props = {
    Props(classOf[BlockActorImpl], peerMsgHandler)
  }

  def apply(peerMsgHandler: ActorRef)(
      implicit context: ActorContext): ActorRef = {
    context.actorOf(props(peerMsgHandler))
  }

}
