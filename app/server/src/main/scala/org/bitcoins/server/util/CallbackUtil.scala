package org.bitcoins.server.util

import akka.actor.ActorSystem
import akka.stream.SharedKillSwitch
import akka.stream.scaladsl.{Sink, Source}
import grizzled.slf4j.Logging
import org.bitcoins.core.api.dlc.wallet.AnyDLCHDWalletApi
import org.bitcoins.core.api.node.{ExternalImplementationNodeType, NodeType}
import org.bitcoins.core.api.wallet.{NeutrinoWalletApi, WalletApi}
import org.bitcoins.node._
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.wallet.WalletNotInitialized
import org.bitcoins.core.gcs.GolombFilter
import org.bitcoins.core.protocol.blockchain.{Block, BlockHeader}
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.crypto.DoubleSha256Digest
import org.bitcoins.node.{
  NodeCallbacks,
  OnBlockHeadersReceived,
  OnBlockReceived,
  OnCompactFiltersReceived,
  OnTxReceived
}
import org.bitcoins.wallet.Wallet

import scala.concurrent.Future

object CallbackUtil extends Logging {

  def createNeutrinoNodeCallbacksForWallet(
      wallet: WalletApi with NeutrinoWalletApi,
      killSwitch: SharedKillSwitch)(implicit
      system: ActorSystem): Future[NodeCallbacks] = {
    import system.dispatcher
    //val killSwitch = KillSwitches.shared("node-callback-kill-switch")
    val txSink = Sink.foreachAsync[Transaction](1) { case tx: Transaction =>
      logger.debug(s"Receiving transaction txid=${tx.txIdBE.hex} as a callback")
      wallet
        .processTransaction(tx, blockHash = None)
        .map(_ => ())
    }

    val compactFilterSink = {
      Sink.foreachAsync[Vector[(DoubleSha256Digest, GolombFilter)]](1) {
        case blockFilters: Vector[(DoubleSha256Digest, GolombFilter)] =>
          logger.debug(
            s"Executing onCompactFilters callback with filter count=${blockFilters.length}")
          wallet
            .processCompactFilters(blockFilters = blockFilters)
            .map(_ => ())
      }
    }

    val blockSink = {
      Sink.foreachAsync[Block](1) { case block: Block =>
        logger.debug(
          s"Executing onBlock callback=${block.blockHeader.hashBE.hex}")
        wallet.processBlock(block).map(_ => ())
      }
    }

    val onHeaderSink = {
      Sink.foreachAsync(1) { headers: Vector[BlockHeader] =>
        logger.debug(
          s"Executing block header with header count=${headers.length}")
        if (headers.isEmpty) {
          Future.unit
        } else {
          wallet.updateUtxoPendingStates().map(_ => ())
        }
      }
    }

    lazy val onTx: OnTxReceived = { tx =>
      Source
        .single(tx)
        .via(killSwitch.flow)
        .runWith(txSink)
        .map(_ => ())
    }
    lazy val onCompactFilters: OnCompactFiltersReceived = { blockFilters =>
      Source
        .single(blockFilters)
        .via(killSwitch.flow)
        .runWith(compactFilterSink)
        .map(_ => ())
        .recover { case _: WalletNotInitialized => () }
    }
    lazy val onBlock: OnBlockReceived = { block =>
      Source
        .single(block)
        .via(killSwitch.flow)
        .runWith(blockSink)
        .map(_ => ())
    }
    lazy val onHeaders: OnBlockHeadersReceived = { headers =>
      Source
        .single(headers)
        .via(killSwitch.flow)
        .runWith(onHeaderSink)
        .map(_ => ())
    }
    Future.successful(
      NodeCallbacks(onTxReceived = Vector(onTx),
                    onBlockReceived = Vector(onBlock),
                    onCompactFiltersReceived = Vector(onCompactFilters),
                    onBlockHeadersReceived = Vector(onHeaders)))
  }

  def createBitcoindNodeCallbacksForWallet(
      wallet: AnyDLCHDWalletApi,
      killSwitch: SharedKillSwitch)(implicit
      system: ActorSystem): Future[NodeCallbacks] = {
    import system.dispatcher
    val txSink = Sink.foreachAsync[Transaction](1) { case tx: Transaction =>
      logger.debug(s"Receiving transaction txid=${tx.txIdBE.hex} as a callback")
      wallet
        .processTransaction(tx, blockHash = None)
        .map(_ => ())
    }
    val onTx: OnTxReceived = { tx =>
      Source
        .single(tx)
        .via(killSwitch.flow)
        .runWith(txSink)
        .map(_ => ())
    }
    Future.successful(NodeCallbacks(onTxReceived = Vector(onTx)))
  }
}
