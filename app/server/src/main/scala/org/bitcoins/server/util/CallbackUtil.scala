package org.bitcoins.server.util

import grizzled.slf4j.Logging
import org.bitcoins.core.api.node.{ExternalImplementationNodeType, NodeType}
import org.bitcoins.node.{
  NodeCallbacks,
  OnBlockHeadersReceived,
  OnBlockReceived,
  OnCompactFiltersReceived,
  OnTxReceived
}
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.wallet.Wallet

import scala.concurrent.{ExecutionContext, Future}

object CallbackUtil extends Logging {

  def createNeutrinoNodeCallbacksForWallet(wallet: Wallet)(implicit
      nodeConf: NodeAppConfig,
      ec: ExecutionContext): Future[NodeCallbacks] = {
    lazy val onTx: OnTxReceived = { tx =>
      logger.debug(s"Receiving transaction txid=${tx.txIdBE.hex} as a callback")
      wallet.processTransaction(tx, blockHashOpt = None).map(_ => ())
    }
    lazy val onCompactFilters: OnCompactFiltersReceived = { blockFilters =>
      logger.debug(
        s"Executing onCompactFilters callback with filter count=${blockFilters.length}")
      wallet
        .processCompactFilters(blockFilters = blockFilters)
        .map(_ => ())
    }
    lazy val onBlock: OnBlockReceived = { block =>
      logger.debug(
        s"Executing onBlock callback=${block.blockHeader.hashBE.hex}")
      wallet.processBlock(block).map(_ => ())
    }
    lazy val onHeaders: OnBlockHeadersReceived = { headers =>
      logger.debug(
        s"Executing block header with header count=${headers.length}")
      if (headers.isEmpty) {
        Future.unit
      } else {
        wallet.updateUtxoPendingStates().map(_ => ())
      }
    }
    nodeConf.nodeType match {
      case NodeType.SpvNode =>
        Future.successful(
          NodeCallbacks(onTxReceived = Vector(onTx),
                        onBlockHeadersReceived = Vector(onHeaders)))
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
}
