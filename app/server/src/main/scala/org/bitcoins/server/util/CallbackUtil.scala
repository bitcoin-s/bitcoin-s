package org.bitcoins.server.util

import grizzled.slf4j.Logging
import org.bitcoins.core.api.dlc.wallet.AnyDLCHDWalletApi
import org.bitcoins.core.api.node.{ExternalImplementationNodeType, NodeType}
import org.bitcoins.core.api.wallet.{NeutrinoWalletApi, WalletApi}
import org.bitcoins.node._
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.wallet.WalletNotInitialized

import scala.concurrent.{ExecutionContext, Future}

object CallbackUtil extends Logging {

  def createNeutrinoNodeCallbacksForWallet(
      wallet: WalletApi with NeutrinoWalletApi)(implicit
      nodeConf: NodeAppConfig,
      ec: ExecutionContext): Future[NodeCallbacks] = {
    lazy val onTx: OnTxReceived = { tx =>
      logger.debug(s"Receiving transaction txid=${tx.txIdBE.hex} as a callback")
      wallet
        .processTransaction(tx, None)
        .map(_ => ())
        .recover { case _: WalletNotInitialized => () }
    }
    lazy val onCompactFilters: OnCompactFiltersReceived = { blockFilters =>
      logger.debug(
        s"Executing onCompactFilters callback with filter count=${blockFilters.length}")
      wallet
        .processCompactFilters(blockFilters = blockFilters)
        .map(_ => ())
        .recover { case _: WalletNotInitialized => () }
    }
    lazy val onBlock: OnBlockReceived = { block =>
      logger.debug(
        s"Executing onBlock callback=${block.blockHeader.hashBE.hex}")
      wallet
        .processBlock(block)
        .map(_ => ())
        .recover { case _: WalletNotInitialized => () }
    }
    lazy val onHeaders: OnBlockHeadersReceived = { headers =>
      logger.debug(
        s"Executing block header with header count=${headers.length}")
      if (headers.isEmpty) {
        Future.unit
      } else {
        wallet
          .updateUtxoPendingStates()
          .map(_ => ())
          .recover { case _: WalletNotInitialized => () }
      }
    }
    nodeConf.nodeType match {
      case NodeType.NeutrinoNode =>
        Future.successful(
          NodeCallbacks(onTxReceived = Vector(onTx),
                        onBlockReceived = Vector(onBlock),
                        onCompactFiltersReceived = Vector(onCompactFilters),
                        onBlockHeadersReceived = Vector(onHeaders)))
      case NodeType.FullNode =>
        Future.failed(new RuntimeException("Not yet implemented"))
      case _: ExternalImplementationNodeType =>
        Future.failed(
          new RuntimeException(
            "Cannot create callbacks for an external implementation"))
    }
  }

  def createBitcoindNodeCallbacksForWallet(wallet: AnyDLCHDWalletApi)(implicit
      ec: ExecutionContext): Future[NodeCallbacks] = {
    val onTx: OnTxReceived = { tx =>
      logger.debug(s"Receiving transaction txid=${tx.txIdBE.hex} as a callback")
      wallet
        .processTransaction(tx, blockHash = None)
        .map(_ => ())
        .recover { case _: WalletNotInitialized => () }
    }
    Future.successful(NodeCallbacks(onTxReceived = Vector(onTx)))
  }
}
