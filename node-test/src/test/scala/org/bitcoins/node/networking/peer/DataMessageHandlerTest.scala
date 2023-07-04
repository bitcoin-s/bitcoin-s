package org.bitcoins.node.networking.peer

import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.core.config.SigNet
import org.bitcoins.core.currency._
import org.bitcoins.core.gcs.{FilterType, GolombFilter}
import org.bitcoins.core.p2p._
import org.bitcoins.core.protocol.blockchain.{Block, BlockHeader}
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.crypto.DoubleSha256Digest
import org.bitcoins.node._
import org.bitcoins.node.networking.peer.NodeState.HeaderSync
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.node.NodeTestWithCachedBitcoindNewest
import org.bitcoins.testkit.node.fixture.NeutrinoNodeConnectedWithBitcoind
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

      val peerManager = node.peerManager
      for {
        chainApi <- node.chainApiFromDb()
        _ = require(peerManager.getPeerData(peer).isDefined)
        dataMessageHandler = DataMessageHandler(
          chainApi = chainApi,
          walletCreationTimeOpt = None,
          queue = peerManager.dataMessageQueueOpt.get,
          peers = peerManager.peers,
          peerMessageSenderApi = peerManager,
          peerDataOpt = peerManager.getPeerData(peer),
          state = HeaderSync(peer),
          filterBatchCache = Set.empty
        )(node.executionContext, node.nodeAppConfig, node.chainConfig)

        // Use signet genesis block header, this should be invalid for regtest
        invalidPayload =
          HeadersMessage(Vector(SigNet.chainParams.genesisBlock.blockHeader))

        // Validate that it causes a failure
        _ <- recoverToSucceededIf[RuntimeException](
          chainApi.processHeaders(invalidPayload.headers))
        // Verify we handle the payload correctly
        _ <- dataMessageHandler.handleDataPayload(invalidPayload, peer)
      } yield succeed
  }

  it must "verify OnBlockReceived callbacks are executed" in {
    param: FixtureParam =>
      val node = param.node
      val bitcoind = param.bitcoind

      val resultP: Promise[Block] = Promise()

      val callback: OnBlockReceived = (block: Block) => {
        Future {
          resultP.success(block)
          ()
        }
      }

      val nodeCallbacks = NodeCallbacks.onBlockReceived(callback)
      node.nodeAppConfig.addCallbacks(nodeCallbacks)

      for {
        hash <- bitcoind.generateToAddress(blocks = 1, junkAddress).map(_.head)
        result <- resultP.future
      } yield assert(result.blockHeader.hashBE == hash)
  }

  it must "verify OnBlockHeadersReceived callbacks are executed" in {
    param: FixtureParam =>
      val node = param.node
      val bitcoind = param.bitcoind

      val resultP: Promise[Vector[BlockHeader]] = Promise()

      val callback: OnBlockHeadersReceived = (headers: Vector[BlockHeader]) => {
        Future {
          if (!resultP.isCompleted) {
            resultP.success(headers)
          }
          ()
        }
      }

      val callbacks = NodeCallbacks.onBlockHeadersReceived(callback)

      node.nodeAppConfig.addCallbacks(callbacks)
      for {
        hash <- bitcoind.generateToAddress(blocks = 1, junkAddress).map(_.head)
        header <- bitcoind.getBlockHeaderRaw(hash)
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

      val resultP: Promise[Transaction] = Promise()

      val callback: OnTxReceived = (tx: Transaction) => {
        Future {
          resultP.success(tx)
          ()
        }
      }

      val nodeCallbacks = NodeCallbacks.onTxReceived(callback)
      val _ = node.nodeAppConfig.addCallbacks(nodeCallbacks)
      for {
        txId <- bitcoind.sendToAddress(junkAddress, 1.bitcoin)
        tx <- bitcoind.getRawTransactionRaw(txId)
        result <- resultP.future
      } yield assert(result == tx)
  }
}
