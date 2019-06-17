package org.bitcoins.node.networking.peer

import org.bitcoins.chain.api.ChainApi
import org.bitcoins.chain.blockchain.ChainHandler
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.models.BlockHeaderDAO
import org.bitcoins.core.util.{BitcoinSLogger, FutureUtil}
import org.bitcoins.core.p2p.{DataPayload, HeadersMessage, InventoryMessage}

import scala.concurrent.{ExecutionContext, Future}
import org.bitcoins.core.protocol.blockchain.Block
import org.bitcoins.core.protocol.blockchain.MerkleBlock
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.p2p.BlockMessage
import org.bitcoins.core.p2p.TransactionMessage
import org.bitcoins.core.p2p.MerkleBlockMessage
import org.bitcoins.node.SpvNodeCallbacks
import org.bitcoins.core.p2p.GetDataMessage

/** This actor is meant to handle a [[org.bitcoins.node.messages.DataPayload]]
  * that a peer to sent to us on the p2p network, for instance, if we a receive a
  * [[HeadersMessage]] we should store those headers in our database
  */
class DataMessageHandler(callbacks: SpvNodeCallbacks)(
    implicit ec: ExecutionContext,
    appConfig: ChainAppConfig)
    extends BitcoinSLogger {

  val callbackNum = callbacks.onBlockReceived.length + callbacks.onMerkleBlockReceived.length + callbacks.onTxReceived.length
  logger.debug(s"Given $callbackNum of callback(s)")

  private val blockHeaderDAO: BlockHeaderDAO = BlockHeaderDAO()

  def handleDataPayload(
      payload: DataPayload,
      peerMsgSender: PeerMessageSender): Future[Unit] = {

    payload match {
      case headersMsg: HeadersMessage =>
        logger.trace(
          s"Received headers message with ${headersMsg.count.toInt} headers")
        val headers = headersMsg.headers
        val chainApi: ChainApi =
          ChainHandler(blockHeaderDAO, chainConfig = appConfig)
        val chainApiF = chainApi.processHeaders(headers)

        chainApiF.map { newApi =>
          val lastHeader = headers.last
          val lastHash = lastHeader.hash
          newApi.getBlockCount.map { count =>
            logger.trace(
              s"Processed headers, most recent has height=$count and hash=$lastHash.")
          }
          peerMsgSender.sendGetHeadersMessage(lastHash)
        }
      case msg: BlockMessage =>
        Future { callbacks.onBlockReceived.foreach(_.apply(msg.block)) }
      case msg: TransactionMessage =>
        Future { callbacks.onTxReceived.foreach(_.apply(msg.transaction)) }
      case msg: MerkleBlockMessage =>
        Future {
          callbacks.onMerkleBlockReceived.foreach(_.apply(msg.merkleBlock))
        }
      case invMsg: InventoryMessage =>
        handleInventoryMsg(invMsg = invMsg, peerMsgSender = peerMsgSender)
    }
  }

  private def handleInventoryMsg(
      invMsg: InventoryMessage,
      peerMsgSender: PeerMessageSender): Future[Unit] = {
    logger.info(s"Received inv=${invMsg}")
    val getData = GetDataMessage(invMsg.inventories)
    peerMsgSender.sendMsg(getData)
    FutureUtil.unit

  }
}

object DataMessageHandler {

  /** Callback for handling a received block */
  type OnBlockReceived = Block => Unit

  /** Callback for handling a received Merkle block */
  type OnMerkleBlockReceived = MerkleBlock => Unit

  /** Callback for handling a received transaction */
  type OnTxReceived = Transaction => Unit

  /** Does nothing */
  def noop[T]: T => Unit = _ => ()

}
