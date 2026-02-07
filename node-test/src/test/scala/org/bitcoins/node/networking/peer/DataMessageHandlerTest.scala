package org.bitcoins.node.networking.peer

import com.typesafe.config.ConfigFactory
import org.apache.pekko.stream.scaladsl.{Sink, Source}
import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.chain.blockchain.ChainHandler
import org.bitcoins.core.api.callback.{OnBlockReceived, OnTxReceived}
import org.bitcoins.core.config.SigNet
import org.bitcoins.core.currency.*
import org.bitcoins.core.gcs.{FilterType, GolombFilter}
import org.bitcoins.core.p2p.HeadersMessage
import org.bitcoins.core.protocol.blockchain.{Block, BlockHeader}
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.node.NodeState.{FilterHeaderSync, HeaderSync}
import org.bitcoins.node.*
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
    BitcoinSTestAppConfig.getNeutrinoWithEmbeddedDbTestConfig(
      () => pgUrl(),
      Vector.empty
    )

  override type FixtureParam = NeutrinoNodeConnectedWithBitcoind

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val outcome: Future[Outcome] = for {
      bitcoind <- cachedBitcoindWithFundsF
      outcome = withNeutrinoNodeConnectedToBitcoindCached(test, bitcoind)(
        using system,
        getFreshConfig
      )
      f <- outcome.toFuture
    } yield f
    new FutureOutcome(outcome)
  }

  it must "catch errors and not fail when processing an invalid payload" in {
    (param: FixtureParam) =>
      val node = param.node

      val peerManager = node.peerManager
      for {
        _ <- AsyncUtil.awaitCondition(() => peerManager.peers.nonEmpty)
        peer = node.peerManager.peers.head
        chainApi <- node.chainApiFromDb()
        _ = require(peerManager.getPeerData(peer).isDefined)
        peerFinder = PeerFinder(
          peerManagerApi = peerManager,
          paramPeers = Vector.empty,
          queue = node
        )(
          using system.dispatcher,
          system,
          node.nodeAppConfig,
          node.chainAppConfig)
        dataMessageHandler = DataMessageHandler(
          chainApi = chainApi,
          walletCreationTimeOpt = None,
          peerManager = peerManager,
          state = HeaderSync(
            syncPeer = peer,
            peerWithServicesDataMap = peerManager.peerWithServicesDataMap,
            waitingForDisconnection = Set.empty,
            peerFinder = peerFinder,
            sentQuery = Instant.now()
          )
        )(using node.executionContext, node.nodeAppConfig, node.chainAppConfig)

        // Use signet genesis block header, this should be invalid for regtest
        invalidPayload =
          HeadersMessage(Vector(SigNet.chainParams.genesisBlock.blockHeader))

        // Validate that it causes a failure
        _ <- recoverToSucceededIf[RuntimeException](
          chainApi.processHeaders(invalidPayload.headers)
        )
        // Verify we handle the payload correctly
        peerData = peerManager.getPeerData(peer).get
        _ <- dataMessageHandler.handleDataPayload(invalidPayload, peerData)
      } yield succeed
  }

  it must "verify OnBlockReceived callbacks are executed" in {
    (param: FixtureParam) =>
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
        hash <- bitcoind.generate(1).map(_.head)
        _ <- NodeTestUtil.awaitAllSync(node, bitcoind)
        _ <- node.downloadBlocks(Vector(hash))
        result = Await.result(resultP.future, 30.seconds)
      } yield assert(result.blockHeader.hashBE == hash)
  }

  it must "verify OnBlockHeadersReceived callbacks are executed" in {
    (param: FixtureParam) =>
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
        hash <- bitcoind.generate(1).map(_.head)
        header <- bitcoind.getBlockHeaderRaw(hash)
        result = Await.result(resultP.future, 30.seconds)
      } yield assert(result == Vector(header))
  }

  it must "verify OnCompactFilterReceived callbacks are executed" in {
    (param: FixtureParam) =>
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
        hash <- bitcoind.generate(1).map(_.head)
        filter <- bitcoind.getBlockFilter(hash, FilterType.Basic)
        result = Await.result(resultP.future, 30.seconds)
      } yield assert(result == Vector((hash, filter.filter)))
  }

  it must "verify OnTxReceived callbacks are executed" in {
    (param: FixtureParam) =>
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
    (param: FixtureParam) =>
      val initNode = param.node
      val bitcoind = param.bitcoind
      val queryWaitTime = 5.second
      val nodeF = getCustomQueryWaitTime(
        initNode = initNode,
        queryWaitTime = queryWaitTime
      )
      for {
        node <- nodeF
        peerManager = node.peerManager
        _ <- bitcoind.generate(1)
        _ <- NodeTestUtil.awaitAllSync(node, bitcoind)
        peer = peerManager.peers.head
        chainApi = ChainHandler.fromDatabase()(
          using executionContext,
          node.chainAppConfig
        )
        _ = require(peerManager.getPeerData(peer).isDefined)
        peerFinder = PeerFinder(
          peerManagerApi = peerManager,
          paramPeers = Vector.empty,
          queue = node
        )(
          using system.dispatcher,
          system,
          node.nodeAppConfig,
          node.chainAppConfig)
        dataMessageHandler = DataMessageHandler(
          chainApi = chainApi,
          walletCreationTimeOpt = None,
          peerManager = peerManager,
          state = HeaderSync(
            syncPeer = peer,
            peerWithServicesDataMap = peerManager.peerWithServicesDataMap,
            waitingForDisconnection = Set.empty,
            peerFinder = peerFinder,
            sentQuery = Instant.now()
          )
        )(using node.executionContext, node.nodeAppConfig, node.chainAppConfig)

        // disconnect our node from bitcoind, then
        // use bitcoind to generate 2 blocks, and then try to send the headers
        // via directly via our queue. We should still be able to process
        // the second header even though our NodeState is FilterHeaderSync
        // this is because the getcfheaders timed out
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
        // now process another header, even though we are in FilterHeaderSync
        // state, we should process the block header since our query timed out
        newDmh1 <- newDmh0.handleDataPayload(payload1, peerData)
        blockCount <- chainApi.getBlockCount()
        _ <- node.stop()
        _ <- node.nodeAppConfig.stop()
      } yield {
        // we should have processed both headers
        assert(blockCount == initBlockCount + 2)
        // should still be FilterHeaderSync state
        assert(newDmh1.state.isInstanceOf[FilterHeaderSync])
      }
  }

  it must "reset sentQuery when processing a header message" in {
    (param: FixtureParam) =>
      val node = param.node
      val bitcoind = param.bitcoind
      val peerManager = node.peerManager
      for {
        _ <- bitcoind.generate(1)
        _ <- NodeTestUtil.awaitAllSync(node, bitcoind)
        peer = peerManager.peers.head
        chainApi = ChainHandler.fromDatabase()(
          using executionContext,
          node.chainAppConfig
        )
        _ = require(peerManager.getPeerData(peer).isDefined)
        peerFinder = PeerFinder(
          peerManagerApi = peerManager,
          paramPeers = Vector.empty,
          queue = node
        )(
          using system.dispatcher,
          system,
          node.nodeAppConfig,
          node.chainAppConfig)
        sentQuery0 = Instant.now()
        dataMessageHandler = DataMessageHandler(
          chainApi = chainApi,
          walletCreationTimeOpt = None,
          peerManager = peerManager,
          state = HeaderSync(
            syncPeer = peer,
            peerWithServicesDataMap = peerManager.peerWithServicesDataMap,
            waitingForDisconnection = Set.empty,
            peerFinder = peerFinder,
            sentQuery = sentQuery0
          )
        )(using node.executionContext, node.nodeAppConfig, node.chainAppConfig)

        // disconnect our node from bitcoind, then
        // use bitcoind to generate 2,000 blocks, and then try to send the headers
        // via directly via our queue. Our internal logic says that we should send another
        // getheaders request to our peer thus reseting the sentQuery time
        peerData = peerManager.getPeerData(peer).get
        _ <- NodeTestUtil.disconnectNode(bitcoind, node)
        // fully functioning node no longer needed so shut it down
        _ <- node.stop()
        _ <- node.nodeAppConfig.stop()
        hashes <- bitcoind.generate(2000)
        blockHeaders <- Source(hashes)
          .mapAsync(FutureUtil.getParallelism)(bitcoind.getBlockHeaderRaw)
          .runWith(Sink.seq)
        payload0 =
          HeadersMessage(blockHeaders.toVector)
        newDmh0 <- dataMessageHandler.handleDataPayload(payload0, peerData)
        _ = assert(newDmh0.state.isInstanceOf[HeaderSync])
      } yield {
        assert(
          newDmh0.state.asInstanceOf[HeaderSync].sentQuery.isAfter(sentQuery0))
      }
  }

  private def getCustomQueryWaitTime(
      initNode: NeutrinoNode,
      queryWaitTime: FiniteDuration
  ): Future[NeutrinoNode] = {

    require(
      initNode.nodeAppConfig.queryWaitTime != queryWaitTime,
      s"maxConnectedPeers must be different"
    )
    // make a custom config, set the inactivity timeout very low
    // so we will disconnect our peer organically
    val str =
      s"""
         |bitcoin-s.node.query-wait-time = $queryWaitTime
         |""".stripMargin
    val config =
      ConfigFactory.parseString(str)
    NodeTestUtil.getStartedNodeCustomConfig(initNode, config)
  }
}
