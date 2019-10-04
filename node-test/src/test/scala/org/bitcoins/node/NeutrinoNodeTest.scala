package org.bitcoins.node

import org.bitcoins.core.crypto.DoubleSha256DigestBE
import org.bitcoins.rpc.client.common.BitcoindVersion
import org.bitcoins.rpc.util.RpcUtil
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.fixtures.UsesExperimentalBitcoind
import org.bitcoins.testkit.node.fixture.NeutrinoNodeConnectedWithBitcoind
import org.bitcoins.testkit.node.{NodeTestUtil, NodeUnitTest}
import org.scalatest.{DoNotDiscover, FutureOutcome}

import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

@DoNotDiscover
class NeutrinoNodeTest extends NodeUnitTest {

  /** Wallet config with data directory set to user temp directory */
  implicit override protected def config: BitcoinSAppConfig =
    BitcoinSTestAppConfig.getNeutrinoTestConfig()

  override type FixtureParam = NeutrinoNodeConnectedWithBitcoind

  override def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withNeutrinoNodeConnectedToBitcoind(test,
                                        Some(BitcoindVersion.Experimental))

  behavior of "NeutrinoNode"

  it must "receive notification that a block occurred on the p2p network" taggedAs (UsesExperimentalBitcoind) in {
    nodeConnectedWithBitcoind: NeutrinoNodeConnectedWithBitcoind =>
      val node = nodeConnectedWithBitcoind.node
      val bitcoind = nodeConnectedWithBitcoind.bitcoind

      val assert1F = for {
        _ <- node.isConnected.map(assert(_))
        a2 <- node.isInitialized.map(assert(_))
      } yield a2

      val hashF: Future[DoubleSha256DigestBE] = bitcoind.getNewAddress
        .flatMap(bitcoind.generateToAddress(1, _))
        .map(_.head)

      //sync our spv node expecting to get that generated hash
      val syncF = for {
        _ <- assert1F
        _ <- hashF
        sync <- node.sync()
      } yield sync

      syncF.flatMap { _ =>
        NodeTestUtil
          .awaitSync(node, bitcoind)
          .map(_ => succeed)
      }
  }

  it must "stay in sync with a bitcoind instance" taggedAs (UsesExperimentalBitcoind) in {
    nodeConnectedWithBitcoind: NeutrinoNodeConnectedWithBitcoind =>
      val node = nodeConnectedWithBitcoind.node
      val bitcoind = nodeConnectedWithBitcoind.bitcoind

      //we need to generate 1 block for bitcoind to consider
      //itself out of IBD. bitcoind will not sendheaders
      //when it believes itself, or it's peer is in IBD
      val gen1F =
        bitcoind.getNewAddress.flatMap(bitcoind.generateToAddress(1, _))

      //this needs to be called to get our peer to send us headers
      //as they happen with the 'sendheaders' message
      //both our spv node and our bitcoind node _should_ both be at the genesis block (regtest)
      //at this point so no actual syncing is happening
      val initSyncF = gen1F.flatMap { hashes =>
        val syncF = node.sync()
        for {
          _ <- syncF
          _ <- NodeTestUtil.awaitBestHash(hashes.head, node)
        } yield ()
      }

      //start generating a block every 10 seconds with bitcoind
      //this should result in 5 blocks
      val startGenF = initSyncF.map { _ =>
        //generate a block every 5 seconds
        //until we have generated 5 total blocks
        genBlockInterval(bitcoind)
      }

      startGenF.flatMap { _ =>
        //we should expect 5 headers have been announced to us via
        //the send headers message.
        def has6BlocksF =
          RpcUtil.retryUntilSatisfiedF(conditionF = () => {
            node
              .chainApiFromDb()
              .flatMap(_.getBlockCount.map(_ == 6))
          }, duration = 1000.millis)

        def has6FilterHeadersF =
          RpcUtil.retryUntilSatisfiedF(conditionF = () => {
            node
              .chainApiFromDb()
              .flatMap(_.getFilterHeaderCount.map(_ == 6))
          }, duration = 1000.millis)

        def has6FiltersF =
          RpcUtil.retryUntilSatisfiedF(conditionF = () => {
            node
              .chainApiFromDb()
              .flatMap(_.getFilterCount.map(_ == 6))
          }, duration = 1000.millis)

        for {
          _ <- has6BlocksF
          _ <- has6FilterHeadersF
          _ <- has6FiltersF
        } yield succeed
      }
  }

}
