package org.bitcoins.node.networking.peer

import org.bitcoins.chain.api.ChainApi
import org.bitcoins.core.util.FutureUtil
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
import org.bitcoins.node.models.BroadcastAbleTransactionDAO
import slick.jdbc.SQLiteProfile
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.core.p2p.TypeIdentifier
import org.bitcoins.core.p2p.MsgUnassigned
import org.bitcoins.db.P2PLogger

/** This actor is meant to handle a [[org.bitcoins.core.p2p.DataPayload DataPayload]]
  * that a peer to sent to us on the p2p network, for instance, if we a receive a
  * [[org.bitcoins.core.p2p.HeadersMessage HeadersMessage]] we should store those headers in our database
  */
class DataMessageHandler(callbacks: SpvNodeCallbacks, chainHandler: ChainApi)(
    implicit ec: ExecutionContext,
    appConfig: NodeAppConfig)
    extends P2PLogger {

  private val callbackNum = callbacks.onBlockReceived.length + callbacks.onMerkleBlockReceived.length + callbacks.onTxReceived.length
  logger.debug(s"Given $callbackNum of callback(s)")

  private val txDAO = BroadcastAbleTransactionDAO(SQLiteProfile)

  def handleDataPayload(
      payload: DataPayload,
      peerMsgSender: PeerMessageSender): Future[Unit] = {

    payload match {
      case getData: GetDataMessage =>
        logger.debug(
          s"Received a getdata message for inventories=${getData.inventories}")
        getData.inventories.foreach { inv =>
          logger.debug(s"Looking for inv=$inv")
          inv.typeIdentifier match {
            case TypeIdentifier.MsgTx =>
              txDAO.findByHash(inv.hash).map {
                case Some(tx) =>
                  peerMsgSender.sendTransactionMessage(tx.transaction)
                case None =>
                  logger.warn(
                    s"Got request to send data with hash=${inv.hash}, but found nothing")
              }
            case other @ (TypeIdentifier.MsgBlock |
                TypeIdentifier.MsgFilteredBlock |
                TypeIdentifier.MsgCompactBlock |
                TypeIdentifier.MsgFilteredWitnessBlock |
                TypeIdentifier.MsgWitnessBlock | TypeIdentifier.MsgWitnessTx) =>
              logger.warn(
                s"Got request to send data type=$other, this is not implemented yet")

            case unassigned: MsgUnassigned =>
              logger.warn(
                s"Received unassigned message we do not understand, msg=${unassigned}")
          }

        }
        FutureUtil.unit
      case headersMsg: HeadersMessage =>
        logger.trace(
          s"Received headers message with ${headersMsg.count.toInt} headers")
        val headers = headersMsg.headers
        val chainApiF = chainHandler.processHeaders(headers)

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
