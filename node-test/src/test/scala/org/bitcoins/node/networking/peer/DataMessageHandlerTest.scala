package org.bitcoins.node.networking.peer

import com.typesafe.config.ConfigFactory
import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.chain.blockchain.ChainHandler
import org.bitcoins.core.config.SigNet
import org.bitcoins.core.currency._
import org.bitcoins.core.gcs.{FilterType, GolombFilter}
import org.bitcoins.core.p2p.HeadersMessage
import org.bitcoins.core.protocol.blockchain.{Block, BlockHeader}
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.node.NodeState.{FilterHeaderSync, HeaderSync}
import org.bitcoins.node._
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.node.fixture.NeutrinoNodeConnectedWithBitcoind
import org.bitcoins.testkit.node.{
  NodeTestUtil,
  NodeTestWithCachedBitcoindNewest
}
import org.scalatest.{FutureOutcome, Outcome}

import java.time.Instant
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{Await, Future, Promise}

class DataMessageHandlerTest extends NodeTestWithCachedBitcoindNewest {

  /** Wallet config with data directory set to user temp directory */
  override protected def getFreshConfig: BitcoinSAppConfig =
    BitcoinSTestAppConfig.getNeutrinoWithEmbeddedDbTestConfig(() => pgUrl(),
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

      val peerManager = node.peerManager
      for {
        _ <- AsyncUtil.awaitCondition(() => peerManager.peers.nonEmpty)
        peer = node.peerManager.peers.head
        chainApi <- node.chainApiFromDb()
        _ = require(peerManager.getPeerData(peer).isDefined)
        peerFinder = PeerFinder(peerManagerApi = peerManager,
                                paramPeers = Vector.empty,
                                queue = node)(system.dispatcher,
                                              system,
                                              node.nodeConfig,
                                              node.chainConfig)
        dataMessageHandler = DataMessageHandler(
          chainApi = chainApi,
          walletCreationTimeOpt = None,
          peerManager = peerManager,
          state = HeaderSync(syncPeer = peer,
                             peerDataMap = peerManager.peerWithServicesDataMap,
                             waitingForDisconnection = Set.empty,
                             peerFinder = peerFinder,
                             sentQuery = Instant.now())
        )(node.executionContext, node.nodeAppConfig, node.chainConfig)

        // Use signet genesis block header, this should be invalid for regtest
        invalidPayload =
          HeadersMessage(Vector(SigNet.chainParams.genesisBlock.blockHeader))

        // Validate that it causes a failure
        _ <- recoverToSucceededIf[RuntimeException](
          chainApi.processHeaders(invalidPayload.headers))
        // Verify we handle the payload correctly
        peerData = peerManager.getPeerData(peer).get
        _ <- dataMessageHandler.handleDataPayload(invalidPayload, peerData)
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

      for {
        _ <- NodeTestUtil.awaitAllSync(node, bitcoind)
        nodeCallbacks = NodeCallbacks.onBlockReceived(callback)
        _ = node.nodeAppConfig.addCallbacks(nodeCallbacks)
        hash <- bitcoind.generateToAddress(blocks = 1, junkAddress).map(_.head)
        _ <- NodeTestUtil.awaitAllSync(node, bitcoind)
        _ <- node.downloadBlocks(Vector(hash))
        result = Await.result(resultP.future, 30.seconds)
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

      for {
        _ <- NodeTestUtil.awaitAllSync(node, bitcoind)
        nodeCallbacks = NodeCallbacks.onBlockHeadersReceived(callback)
        _ = node.nodeAppConfig.addCallbacks(nodeCallbacks)
        hash <- bitcoind.generateToAddress(blocks = 1, junkAddress).map(_.head)
        header <- bitcoind.getBlockHeaderRaw(hash)
        result = Await.result(resultP.future, 30.seconds)
      } yield assert(result == Vector(header))
  }

  it must "verify OnCompactFilterReceived callbacks are executed" in {
    param: FixtureParam =>
      val node = param.node
      val bitcoind = param.bitcoind

      val resultP: Promise[Vector[(DoubleSha256DigestBE, GolombFilter)]] =
        Promise()
      val callback: OnCompactFiltersReceived = {
        (filters: Vector[(DoubleSha256DigestBE, GolombFilter)]) =>
          Future {
            resultP.success(filters)
            ()
          }
      }

      for {
        _ <- NodeTestUtil.awaitAllSync(node, bitcoind)
        nodeCallbacks = NodeCallbacks.onCompactFilterReceived(callback)
        _ = node.nodeAppConfig.addCallbacks(nodeCallbacks)
        _ <- AsyncUtil.nonBlockingSleep(2.seconds)
        hash <- bitcoind.generateToAddress(blocks = 1, junkAddress).map(_.head)
        filter <- bitcoind.getBlockFilter(hash, FilterType.Basic)
        result = Await.result(resultP.future, 30.seconds)
      } yield assert(result == Vector((hash, filter.filter)))
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

      for {
        _ <- NodeTestUtil.awaitAllSync(node, bitcoind)
        nodeCallbacks = NodeCallbacks.onTxReceived(callback)
        _ = node.nodeAppConfig.addCallbacks(nodeCallbacks)
        txId <- bitcoind.sendToAddress(junkAddress, 1.bitcoin)
        tx <- bitcoind.getRawTransactionRaw(txId)
        result = Await.result(resultP.future, 30.seconds)
      } yield assert(result == tx)
  }

  it must "detect bitcoin-s.node.query-wait-time timing out" in {
    param: FixtureParam =>
      val initNode = param.node
      val bitcoind = param.bitcoind
      val queryWaitTime = 5.second
      val nodeF = getCustomQueryWaitTime(initNode = initNode,
                                         queryWaitTime = queryWaitTime)
      for {
        node <- nodeF
        peerManager = node.peerManager
        _ <- bitcoind.generate(1)
        _ <- NodeTestUtil.awaitAllSync(node, bitcoind)
        peer = peerManager.peers.head
        chainApi = ChainHandler.fromDatabase()(executionContext,
                                               node.chainConfig)
        _ = require(peerManager.getPeerData(peer).isDefined)
        peerFinder = PeerFinder(peerManagerApi = peerManager,
                                paramPeers = Vector.empty,
                                queue = node)(system.dispatcher,
                                              system,
                                              node.nodeConfig,
                                              node.chainConfig)
        dataMessageHandler = DataMessageHandler(
          chainApi = chainApi,
          walletCreationTimeOpt = None,
          peerManager = peerManager,
          state = HeaderSync(syncPeer = peer,
                             peerDataMap = peerManager.peerWithServicesDataMap,
                             waitingForDisconnection = Set.empty,
                             peerFinder = peerFinder,
                             sentQuery = Instant.now())
        )(node.executionContext, node.nodeAppConfig, node.chainConfig)

        //disconnect our node from bitcoind, then
        //use bitcoind to generate 2 blocks, and then try to send the headers
        //via directly via our queue. We should still be able to process
        //the second header even though our NodeState is FilterHeaderSync
        //this is because the getcfheaders timed out
        peerData = peerManager.getPeerData(peer).get
        _ <- NodeTestUtil.disconnectNode(bitcoind, node)
        initBlockCount <- chainApi.getBlockCount()
        hashes <- bitcoind.generate(2)
        blockHeader0 <- bitcoind.getBlockHeaderRaw(hashes.head)
        blockHeader1 <- bitcoind.getBlockHeaderRaw(hashes(1))
        payload0 =
          HeadersMessage(Vector(blockHeader0))
        payload1 = HeadersMessage(Vector(blockHeader1))
        newDmh0 <- dataMessageHandler.handleDataPayload(payload0, peerData)
        _ = assert(newDmh0.state.isInstanceOf[FilterHeaderSync])
        _ <- AsyncUtil.nonBlockingSleep(queryWaitTime)
        //now process another header, even though we are in FilterHeaderSync
        //state, we should process the block header since our query timed out
        newDmh1 <- newDmh0.handleDataPayload(payload1, peerData)
        blockCount <- chainApi.getBlockCount()
        _ <- node.stop()
        _ <- node.nodeConfig.stop()
      } yield {
        //we should have processed both headers
        assert(blockCount == initBlockCount + 2)
        //should still be FilterHeaderSync state
        assert(newDmh1.state.isInstanceOf[FilterHeaderSync])
      }
  }

  private def getCustomQueryWaitTime(
      initNode: NeutrinoNode,
      queryWaitTime: FiniteDuration): Future[NeutrinoNode] = {

    require(initNode.nodeConfig.queryWaitTime != queryWaitTime,
            s"maxConnectedPeers must be different")
    //make a custom config, set the inactivity timeout very low
    //so we will disconnect our peer organically
    val str =
      s"""
         |bitcoin-s.node.query-wait-time = $queryWaitTime
         |""".stripMargin
    val config =
      ConfigFactory.parseString(str)
    NodeTestUtil.getStartedNodeCustomConfig(initNode, config)
  }
}
