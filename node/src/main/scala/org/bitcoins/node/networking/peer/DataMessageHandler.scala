package org.bitcoins.node.networking.peer

import akka.actor.{ActorRef, ActorRefFactory, Props}
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.node.db.DbConfig
import org.bitcoins.node.messages.{DataPayload, HeadersMessage}
import org.bitcoins.node.models.BlockHeaderDAO
import org.bitcoins.node.util.BitcoinSpvNodeUtil

import scala.concurrent.ExecutionContext

/** This actor is meant to handle a [[org.bitcoins.node.messages.DataPayload]]
  * that a peer to sent to us on the p2p network, for instance, if we a receive a
  * [[HeadersMessage]] we should store those headers in our database
  */
class DataMessageHandler(dbConfig: DbConfig)(implicit ec: ExecutionContext)
    extends BitcoinSLogger {

  private val blockHeaderDAO = BlockHeaderDAO(dbConfig)

  def handleDataPayload(
      payload: DataPayload,
      peerMsgSender: PeerMessageSender): Unit = payload match {
    case headersMsg: HeadersMessage =>
      blockHeaderDAO.upsertAll(headersMsg.headers.toVector)
  }
}

object DataMessageHandler {

  def props(dbConfig: DbConfig): Props = {
    Props(classOf[DataMessageHandler], dbConfig)
  }

  def apply(dbConfig: DbConfig)(implicit ref: ActorRefFactory): ActorRef = {
    ref.actorOf(props(dbConfig), BitcoinSpvNodeUtil.createActorName(getClass))
  }
}
