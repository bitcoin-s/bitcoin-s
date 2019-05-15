package org.bitcoins.node.networking.peer

import org.bitcoins.chain.blockchain.ChainHandler
import org.bitcoins.chain.models.BlockHeaderDAO
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.db.AppConfig
import org.bitcoins.node.messages.{DataPayload, HeadersMessage}

import scala.concurrent.{ExecutionContext, Future}

/** This actor is meant to handle a [[org.bitcoins.node.messages.DataPayload]]
  * that a peer to sent to us on the p2p network, for instance, if we a receive a
  * [[HeadersMessage]] we should store those headers in our database
  */
class DataMessageHandler(appConfig: AppConfig)(implicit ec: ExecutionContext)
    extends BitcoinSLogger {

  private val blockHeaderDAO = BlockHeaderDAO(appConfig)

  def handleDataPayload(
      payload: DataPayload,
      peerMsgSender: PeerMessageSender): Future[Unit] = payload match {
    case headersMsg: HeadersMessage =>
      val chainApi = ChainHandler(blockHeaderDAO, chainConfig = appConfig)
      val headers = headersMsg.headers

      val chainApiF = chainApi.processHeaders(headers)

      chainApiF.map { chainApi =>
        val lastHash = headers.last.hash
        peerMsgSender.sendGetHeadersMessage(lastHash)
      }

  }
}
