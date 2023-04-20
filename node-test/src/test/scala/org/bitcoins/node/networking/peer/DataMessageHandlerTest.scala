package org.bitcoins.node.networking.peer

import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.core.config.SigNet
import org.bitcoins.core.currency._
import org.bitcoins.core.gcs.{FilterType, GolombFilter}
import org.bitcoins.core.p2p._
import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.protocol.blockchain.{Block, BlockHeader}
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.crypto.DoubleSha256Digest
import org.bitcoins.node._
import org.bitcoins.node.networking.peer.DataMessageHandlerState.HeaderSync
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.node.{NodeTestWithCachedBitcoindNewest}
import org.bitcoins.testkit.node.fixture.{NeutrinoNodeConnectedWithBitcoind}
import org.scalatest.{FutureOutcome, Outcome}

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Future, Promise}

class DataMessageHandlerTest extends NodeTestWithCachedBitcoindNewest {

  /** Wallet config with data directory set to user temp directory */
  override protected def getFreshConfig: BitcoinSAppConfig =
    BitcoinSTestAppConfig.getNeutrinoWithEmbeddedDbTestConfig(pgUrl,
                                                              Vector.empty)

  override type FixtureParam = NeutrinoNodeConnectedWithBitcoind

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val outcome: Future[Outcome] = for {
      bitcoind <- cachedBitcoindWithFundsF
      outcome = withNeutrinoNodeConnectedToBitcoindCached(test, bitcoind)(
        system,
        getFreshConfig)
      f <- outcome.toFuture
    } yield f
    new FutureOutcome(outcome)
  }

  it must "catch errors and not fail when processing an invalid payload" in {
    param: FixtureParam =>
      val node = param.node
      val peer = node.peerManager.peers.head

      val senderF = node.peerMsgSendersF.map(_.head)
      for {
        chainApi <- node.chainApiFromDb()
        dataMessageHandler = DataMessageHandler(
          chainApi = chainApi,
          walletCreationTimeOpt = None,
          peerManager = node.peerManager,
          state = HeaderSync,
          initialSyncDone = None,
          filterBatchCache = Set.empty,
          syncPeer = Some(peer)
        )(node.executionContext, node.nodeAppConfig, node.chainConfig)

        // Use signet genesis block header, this should be invalid for regtest
        invalidPayload =
          HeadersMessage(Vector(SigNet.chainParams.genesisBlock.blockHeader))

        // Validate that it causes a failure
        _ <- recoverToSucceededIf[RuntimeException](
          chainApi.processHeaders(invalidPayload.headers))

        sender <- senderF
        // Verify we handle the payload correctly
        _ <- dataMessageHandler.handleDataPayload(invalidPayload, sender, peer)
      } yield succeed
  }

  it must "verify OnBlockReceived callbacks are executed" in {
    param: FixtureParam =>
      val node = param.node
      val bitcoind = param.bitcoind
      val peer = node.peerManager.peers.head

      val resultP: Promise[Block] = Promise()

      val callback: OnBlockReceived = (block: Block) => {
        Future {
          resultP.success(block)
          ()
        }
      }
      val senderF = node.peerMsgSendersF.map(_.head)

      for {
        hash <- bitcoind.generateToAddress(blocks = 1, junkAddress).map(_.head)
        block <- bitcoind.getBlockRaw(hash)

        payload = BlockMessage(block)

        nodeCallbacks = NodeCallbacks.onBlockReceived(callback)
        _ = node.nodeAppConfig.addCallbacks(nodeCallbacks)
        chainApi <- node.chainApiFromDb()
        dataMessageHandler =
          DataMessageHandler(
            chainApi = chainApi,
            walletCreationTimeOpt = None,
            peerManager = node.peerManager,
            state = HeaderSync,
            initialSyncDone = None,
            filterBatchCache = Set.empty,
            syncPeer = Some(peer)
          )(node.executionContext, node.nodeAppConfig, node.chainConfig)
        sender <- senderF
        _ <- dataMessageHandler.handleDataPayload(payload, sender, peer)
        result <- resultP.future
      } yield assert(result == block)
  }

  it must "verify OnBlockHeadersReceived callbacks are executed" in {
    param: FixtureParam =>
      val node = param.node
      val bitcoind = param.bitcoind
      val peer = node.peerManager.peers.head

      val resultP: Promise[Vector[BlockHeader]] = Promise()

      val callback: OnBlockHeadersReceived = (headers: Vector[BlockHeader]) => {
        Future {
          if (!resultP.isCompleted) {
            resultP.success(headers)
          }
          ()
        }
      }

      val senderF = node.peerMsgSendersF.map(_.head)
      for {
        hash <- bitcoind.generateToAddress(blocks = 1, junkAddress).map(_.head)
        header <- bitcoind.getBlockHeaderRaw(hash)

        payload = HeadersMessage(CompactSizeUInt.one, Vector(header))

        callbacks = NodeCallbacks.onBlockHeadersReceived(callback)

        _ = node.nodeAppConfig.addCallbacks(callbacks)
        chainApi <- node.chainApiFromDb()
        dataMessageHandler =
          DataMessageHandler(
            chainApi = chainApi,
            walletCreationTimeOpt = None,
            peerManager = node.peerManager,
            state = HeaderSync,
            initialSyncDone = None,
            filterBatchCache = Set.empty,
            syncPeer = Some(peer)
          )(node.executionContext, node.nodeAppConfig, node.chainConfig)
        sender <- senderF
        _ <- dataMessageHandler.handleDataPayload(payload, sender, peer)
        result <- resultP.future
      } yield assert(result == Vector(header))
  }

  it must "verify OnCompactFilterReceived callbacks are executed" in {
    param: FixtureParam =>
      val node = param.node
      val bitcoind = param.bitcoind

      val resultP: Promise[Vector[(DoubleSha256Digest, GolombFilter)]] =
        Promise()
      val callback: OnCompactFiltersReceived = {
        (filters: Vector[(DoubleSha256Digest, GolombFilter)]) =>
          Future {
            resultP.success(filters)
            ()
          }
      }
      val nodeCallbacks = NodeCallbacks.onCompactFilterReceived(callback)
      val _ = node.nodeAppConfig.addCallbacks(nodeCallbacks)
      for {
        _ <- AsyncUtil.nonBlockingSleep(2.seconds)
        hash <- bitcoind.generateToAddress(blocks = 1, junkAddress).map(_.head)
        filter <- bitcoind.getBlockFilter(hash, FilterType.Basic)
        result <- resultP.future
      } yield assert(result == Vector((hash.flip, filter.filter)))
  }

  it must "verify OnTxReceived callbacks are executed" in {
    param: FixtureParam =>
      val node = param.node
      val bitcoind = param.bitcoind
      val peer = node.peerManager.peers.head

      val resultP: Promise[Transaction] = Promise()

      val callback: OnTxReceived = (tx: Transaction) => {
        Future {
          resultP.success(tx)
          ()
        }
      }
      val senderF = node.peerMsgSendersF.map(_.head)

      for {

        txId <- bitcoind.sendToAddress(junkAddress, 1.bitcoin)
        tx <- bitcoind.getRawTransactionRaw(txId)

        payload = TransactionMessage(tx)

        nodeCallbacks = NodeCallbacks.onTxReceived(callback)
        _ = node.nodeAppConfig.addCallbacks(nodeCallbacks)
        chainApi <- node.chainApiFromDb()
        dataMessageHandler =
          DataMessageHandler(
            chainApi = chainApi,
            walletCreationTimeOpt = None,
            peerManager = node.peerManager,
            state = HeaderSync,
            initialSyncDone = None,
            filterBatchCache = Set.empty,
            syncPeer = Some(peer)
          )(node.executionContext, node.nodeAppConfig, node.chainConfig)
        sender <- senderF
        _ <- dataMessageHandler.handleDataPayload(payload, sender, peer)
        result <- resultP.future
      } yield assert(result == tx)
  }
}
