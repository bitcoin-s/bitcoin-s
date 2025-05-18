package org.bitcoins.server.util

import org.apache.pekko.actor.ActorSystem
import org.bitcoins.commons.util.BitcoinSLogger
import org.bitcoins.core.api.callback.{OnBlockReceived, OnTxReceived}
import org.bitcoins.core.api.dlc.wallet.DLCNeutrinoHDWalletApi
import org.bitcoins.core.api.wallet.{NeutrinoWalletApi, WalletApi}
import org.bitcoins.node.*
import org.bitcoins.node.callback.NodeCallbackStreamManager
import org.bitcoins.rpc.callback.{
  BitcoindCallbackStreamManager,
  BitcoindCallbacks
}

import scala.concurrent.Future

object CallbackUtil extends BitcoinSLogger {

  def createNeutrinoNodeCallbacksForWallet(
      wallet: WalletApi & NeutrinoWalletApi
  )(implicit system: ActorSystem): Future[NodeCallbackStreamManager] = {
    import system.dispatcher

    lazy val onTx: OnTxReceived = { tx =>
      wallet.transactionProcessing
        .processTransaction(tx, blockHashWithConfsOpt = None)
        .map(_ => ())
    }
    lazy val onCompactFilters: OnCompactFiltersReceived = { blockFilters =>
      logger.debug(
        s"Executing onCompactFilters callback with filter count=${blockFilters.length}"
      )
      wallet
        .processCompactFilters(blockFilters = blockFilters)
        .map(_ => ())
    }
    lazy val onBlock: OnBlockReceived = { block =>
      logger.debug(
        s"Executing onBlock callback=${block.blockHeader.hashBE.hex}"
      )
      wallet.transactionProcessing
        .processBlock(block)
        .map(_ => ())
    }
    lazy val onHeaders: OnBlockHeadersReceived = { headers =>
      if (headers.isEmpty) {
        Future.unit
      } else {
        wallet.utxoHandling.updateUtxoPendingStates().map(_ => ())
      }
    }

    val callbacks = NodeCallbacks(
      onTxReceived = Vector(onTx),
      onBlockReceived = Vector(onBlock),
      onCompactFiltersReceived = Vector(onCompactFilters),
      onBlockHeadersReceived = Vector(onHeaders)
    )

    val streamManager = NodeCallbackStreamManager(callbacks)
    Future.successful(streamManager)
  }

  def createBitcoindNodeCallbacksForWallet(
      wallet: DLCNeutrinoHDWalletApi
  )(implicit system: ActorSystem): Future[BitcoindCallbackStreamManager] = {
    import system.dispatcher
    val onTx: OnTxReceived = { tx =>
      logger.debug(s"Receiving transaction txid=${tx.txIdBE.hex} as a callback")
      wallet.transactionProcessing
        .processTransaction(tx, blockHashWithConfsOpt = None)
        .map(_ => ())
    }

    val onBlock: OnBlockReceived = { block =>
      wallet.transactionProcessing
        .processBlock(block)
        .map(_ => ())
    }
    val callbacks = BitcoindCallbacks(onTxReceived = Vector(onTx),
                                      onBlockReceived = Vector(onBlock))
    val streamManager = BitcoindCallbackStreamManager(callbacks)
    Future.successful(streamManager)
  }
}
