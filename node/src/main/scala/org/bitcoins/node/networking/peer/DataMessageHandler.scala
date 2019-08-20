package org.bitcoins.node.networking.peer

import org.bitcoins.chain.api.ChainApi
import org.bitcoins.core.p2p.{Inventory, MsgUnassigned, TypeIdentifier, _}
import org.bitcoins.core.protocol.blockchain.{Block, MerkleBlock}
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.node.P2PLogger
import org.bitcoins.node.SpvNodeCallbacks
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.models.BroadcastAbleTransactionDAO
import slick.jdbc.SQLiteProfile

import scala.concurrent.{ExecutionContext, Future}

/** This actor is meant to handle a [[org.bitcoins.core.p2p.DataPayload DataPayload]]
  * that a peer to sent to us on the p2p network, for instance, if we a receive a
  * [[org.bitcoins.core.p2p.HeadersMessage HeadersMessage]] we should store those headers in our database
  */

class DataMessageHandler(chainApi: ChainApi, callbacks: SpvNodeCallbacks)(
    implicit ec: ExecutionContext,
    appConfig: NodeAppConfig)
    extends P2PLogger {

  private val txDAO = BroadcastAbleTransactionDAO(SQLiteProfile)

  def handleDataPayload(
      payload: DataPayload,
      peerMsgSender: PeerMessageSender): Future[DataMessageHandler] = {

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
        Future.successful(this)
      case HeadersMessage(count, headers) =>
        logger.debug(s"Received headers message with ${count.toInt} headers")
        logger.trace(
          s"Received headers=${headers.map(_.hashBE.hex).mkString("[", ",", "]")}")
        val chainApiF = chainApi.processHeaders(headers)

        logger.trace(s"Requesting data for headers=${headers.length}")
        peerMsgSender.sendGetDataMessage(headers: _*)

        val getHeadersF = chainApiF
          .map { newApi =>
            if (headers.nonEmpty) {

              val lastHeader = headers.last
              val lastHash = lastHeader.hash
              newApi.getBlockCount.map { count =>
                logger.trace(
                  s"Processed headers, most recent has height=$count and hash=$lastHash.")
              }

              if (count.toInt == HeadersMessage.MaxHeadersCount) {
                logger.error(
                  s"Received maximum amount of headers in one header message. This means we are not synced, requesting more")
                peerMsgSender.sendGetHeadersMessage(lastHash)
              } else {
                logger.debug(
                  List(s"Received headers=${count.toInt} in one message,",
                       "which is less than max. This means we are synced,",
                       "not requesting more.")
                    .mkString(" "))
              }

            }
          }

        getHeadersF.failed.map { err =>
          logger.error(s"Error when processing headers message", err)
        }

        for {
          newApi <- chainApiF
          _ <- getHeadersF
        } yield {
          new DataMessageHandler(newApi, callbacks)
        }
      case msg: BlockMessage =>
        Future {
          callbacks.onBlockReceived.foreach(_.apply(msg.block))
          this
        }
      case TransactionMessage(tx) =>
        val belongsToMerkle =
          MerkleBuffers.putTx(tx, callbacks.onMerkleBlockReceived)
        if (belongsToMerkle) {
          logger.trace(
            s"Transaction=${tx.txIdBE} belongs to merkleblock, not calling callbacks")
          Future.successful(this)
        } else {
          logger.trace(
            s"Transaction=${tx.txIdBE} does not belong to merkleblock, processing given callbacks")
          Future {
            callbacks.onTxReceived.foreach(_.apply(tx))
            this
          }
        }
      case MerkleBlockMessage(merkleBlock) =>
        MerkleBuffers.putMerkle(merkleBlock)
        Future.successful(this)
      case invMsg: InventoryMessage =>
        handleInventoryMsg(invMsg = invMsg, peerMsgSender = peerMsgSender)
    }
  }

  private def handleInventoryMsg(
      invMsg: InventoryMessage,
      peerMsgSender: PeerMessageSender): Future[DataMessageHandler] = {
    logger.info(s"Received inv=${invMsg}")
    val getData = GetDataMessage(invMsg.inventories.map {
      case Inventory(TypeIdentifier.MsgBlock, hash) =>
        Inventory(TypeIdentifier.MsgFilteredBlock, hash)
      case other: Inventory => other
    })
    peerMsgSender.sendMsg(getData)
    Future.successful(this)

  }
}

object DataMessageHandler {

  /** Callback for handling a received block */
  type OnBlockReceived = Block => Unit

  /** Callback for handling a received Merkle block with its corresponding TXs */
  type OnMerkleBlockReceived = (MerkleBlock, Vector[Transaction]) => Unit

  /** Callback for handling a received transaction */
  type OnTxReceived = Transaction => Unit

  /** Does nothing */
  def noop[T]: T => Unit = _ => ()

}
