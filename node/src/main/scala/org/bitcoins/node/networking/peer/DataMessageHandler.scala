package org.bitcoins.node.networking.peer

import akka.actor.{ActorRef, ActorRefFactory, Props}
import org.bitcoins.chain.models.BlockHeaderDAO
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.node.messages.{DataPayload, HeadersMessage}
import org.bitcoins.node.util.BitcoinSpvNodeUtil

import scala.concurrent.ExecutionContext
import org.bitcoins.db.AppConfig

/** This actor is meant to handle a [[org.bitcoins.node.messages.DataPayload]]
  * that a peer to sent to us on the p2p network, for instance, if we a receive a
  * [[HeadersMessage]] we should store those headers in our database
  */
class DataMessageHandler(appConfig: AppConfig)(implicit ec: ExecutionContext)
    extends BitcoinSLogger {

  private val blockHeaderDAO = BlockHeaderDAO(appConfig)

  def handleDataPayload(
      payload: DataPayload,
      peerMsgSender: PeerMessageSender): Unit = payload match {
    case headersMsg: HeadersMessage =>
    //blockHeaderDAO.upsertAll(headersMsg.headers.toVector)
  }
}

object DataMessageHandler {

  def props(): Props = {
    Props(classOf[DataMessageHandler])
  }

  def apply()(implicit ref: ActorRefFactory): ActorRef = {
    ref.actorOf(props(), BitcoinSpvNodeUtil.createActorName(getClass))
  }
}
