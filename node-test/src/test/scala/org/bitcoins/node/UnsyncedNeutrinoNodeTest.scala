package org.bitcoins.node

import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.node.fixture.NeutrinoNodeConnectedWithBitcoinds
import org.bitcoins.testkit.node.{NodeTestUtil, NodeTestWithCachedBitcoindPair}
import org.bitcoins.testkit.util.AkkaUtil
import org.scalatest.{Assertion, FutureOutcome, Outcome}

import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

class UnsyncedNeutrinoNodeTest extends NodeTestWithCachedBitcoindPair {

  override protected def getFreshConfig: BitcoinSAppConfig =
    BitcoinSTestAppConfig.getMultiPeerNeutrinoWithEmbeddedDbTestConfig(pgUrl)

  override type FixtureParam = NeutrinoNodeConnectedWithBitcoinds

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val outcomeF: Future[Outcome] = for {
      bitcoinds <- clientsF
      outcome = withUnsyncedNeutrinoNodeConnectedToBitcoinds(
        test,
        bitcoinds.toVector)(system, getFreshConfig)
      f <- outcome.toFuture
    } yield f
    new FutureOutcome(outcomeF)
  }

  behavior of "Neutrino Node"

  //assuming both peers are in sync with each other
  it must "be able to sync" in { nodeConnectedWithBitcoinds =>
    val node = nodeConnectedWithBitcoinds.node
    val bitcoinds = nodeConnectedWithBitcoinds.bitcoinds
    val peerManager = node.peerManager
    def peers = peerManager.peers

    val connAndInit = for {
      _ <- AsyncUtil
        .retryUntilSatisfied(peers.size == 2, interval = 1.second, maxTries = 5)
      _ <- Future
        .sequence(peers.map(peerManager.isConnected))
        .flatMap(p => assert(p.forall(_ == true)))
      res <- Future
        .sequence(peers.map(peerManager.isConnected))
        .flatMap(p => assert(p.forall(_ == true)))
    } yield res

    val remotesInSync: Future[Assertion] = for {
      h1 <- bitcoinds(0).getBestBlockHash
      h2 <- bitcoinds(1).getBestBlockHash
    } yield assert(h1 == h2)

    def hashF: Future[DoubleSha256DigestBE] = bitcoinds.head.getNewAddress
      .flatMap(bitcoinds.head.generateToAddress(1, _))
      .map(_.head)

    for {
      _ <- connAndInit
      _ <- remotesInSync
      _ <- node.sync()
      _ <- NodeTestUtil.awaitAllSync(node, bitcoinds.head)
      _ <- NodeTestUtil.awaitAllSync(node, bitcoinds.last)
      cnt <- node.getBestBlockHash()
      cnt2 <- bitcoinds.head.getBestBlockHash
      cnt3 <- bitcoinds.head.getBestBlockHash
      bestHash <- hashF
      _ <- NodeTestUtil.awaitAllSync(node, bitcoinds.head)
      _ <- NodeTestUtil.awaitAllSync(node, bitcoinds.last)
      cnt4 <- node.getBestBlockHash()
      cnt5 <- bitcoinds.head.getBestBlockHash
      cnt6 <- bitcoinds.head.getBestBlockHash
    } yield {
      assert(cnt == cnt2 && cnt == cnt3)
      assert(cnt4 == cnt5 && cnt4 == cnt6 && cnt4 == bestHash)
    }
  }

  it must "switch to different peer and sync if current is unavailable" in {
    nodeConnectedWithBitcoinds =>
      val node = nodeConnectedWithBitcoinds.node
      val bitcoinds = nodeConnectedWithBitcoinds.bitcoinds
      val peerManager = node.peerManager

      def peers = peerManager.peers

      val bitcoindPeersF =
        Future.sequence(bitcoinds.map(NodeTestUtil.getBitcoindPeer))

      def connAndInit: Future[Assertion] = for {
        _ <- AsyncUtil
          .retryUntilSatisfied(peers.size == 2,
                               interval = 1.second,
                               maxTries = 5)
        _ <- Future
          .sequence(peers.map(peerManager.isConnected))
          .flatMap(p => assert(p.forall(_ == true)))
        res <- Future
          .sequence(peers.map(peerManager.isConnected))
          .flatMap(p => assert(p.forall(_ == true)))
      } yield res

      val remotesInSync: Future[Assertion] = for {
        h1 <- bitcoinds(0).getBestBlockHash
        h2 <- bitcoinds(1).getBestBlockHash
      } yield assert(h1 == h2)

      for {
        bitcoindPeers <- bitcoindPeersF
        _ <- remotesInSync
        zipped = bitcoinds.zip(bitcoindPeers)
        _ <- connAndInit
        _ <- node.sync()
        sync1 = zipped.find(_._2 == node.getDataMessageHandler.syncPeer.get).get
        h1 <- sync1._1.getBestHashBlockHeight()
        _ <- sync1._1.stop()
        // generating new blocks from the other bitcoind instance
        other = bitcoinds.filterNot(_ == sync1._1).head
        _ <- other.getNewAddress.flatMap(other.generateToAddress(10, _))
        _ <- node.sync()
        _ <- AkkaUtil.nonBlockingSleep(3.seconds)
        sync2 = zipped.find(_._2 == node.getDataMessageHandler.syncPeer.get).get
        _ <- NodeTestUtil.awaitSync(node, sync2._1)
        h2 <- sync2._1.getBestHashBlockHeight()
        //these are shared fixtures so, starting it again
        _ <- sync1._1.start()
      } yield {
        assert(sync1._2 != sync2._2 && h2 - h1 == 10)
      }
  }
}
