package org.bitcoins.node

import akka.actor.Cancellable
import org.bitcoins.core.protocol.blockchain.Block
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.rpc.client.common.BitcoindVersion
import org.bitcoins.rpc.util.RpcUtil
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.fixtures.UsesExperimentalBitcoind
import org.bitcoins.testkit.node.{
  NeutrinoNodeFundedWalletBitcoind,
  NodeTestUtil,
  NodeUnitTest
}
import org.scalatest.FutureOutcome

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Future, Promise}

class NeutrinoNodeTest extends NodeUnitTest {

  /** Wallet config with data directory set to user temp directory */
  implicit override protected def getFreshConfig: BitcoinSAppConfig =
    BitcoinSTestAppConfig.getNeutrinoWithEmbeddedDbTestConfig(pgUrl)

  override type FixtureParam = NeutrinoNodeFundedWalletBitcoind

  override def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withNeutrinoNodeFundedWalletBitcoind(test,
                                         getBIP39PasswordOpt(),
                                         Some(BitcoindVersion.V21))

  private var assertionP: Promise[Boolean] = Promise()
  after {
    //reset assertion after a test runs, because we
    //are doing mutation to work around our callback
    //limitations, we can't currently modify callbacks
    //after a NeutrinoNode is constructed :-(
    assertionP = Promise()
  }
  //what is going on here??
  private val utxos: Set[ScriptPubKey] = Set.empty

  private def blockCallback(block: Block): Future[Unit] = {
    val scriptPubKeys =
      block.transactions.flatMap(tx => tx.outputs.map(_.scriptPubKey)).toSet
    assertionP
      //is this trivially false always?
      .success(utxos.intersect(scriptPubKeys) == utxos)
      .future
      .map(_ => ())
  }

  def callbacks: NodeCallbacks = {
    NodeCallbacks(onBlockReceived = Vector(blockCallback(_)))
  }

  behavior of "NeutrinoNode"

  it must "receive notification that a block occurred on the p2p network" taggedAs UsesExperimentalBitcoind in {
    nodeConnectedWithBitcoind: NeutrinoNodeFundedWalletBitcoind =>
      val node = nodeConnectedWithBitcoind.node

      val _ = node.nodeAppConfig.addCallbacks(callbacks)
      val bitcoind = nodeConnectedWithBitcoind.bitcoindRpc

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

  it must "stay in sync with a bitcoind instance" taggedAs UsesExperimentalBitcoind in {
    nodeConnectedWithBitcoind: NeutrinoNodeFundedWalletBitcoind =>
      val node = nodeConnectedWithBitcoind.node
      val bitcoind = nodeConnectedWithBitcoind.bitcoindRpc

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
      val startGenF: Future[Cancellable] = initSyncF.map { _ =>
        //generate a block every 5 seconds
        //until we have generated 5 total blocks
        genBlockInterval(bitcoind)
      }

      startGenF.flatMap { cancellable =>
        //we should expect 5 headers have been announced to us via
        //the send headers message.
        val ExpectedCount = 119

        def hasBlocksF =
          RpcUtil.retryUntilSatisfiedF(conditionF = () => {
                                         node
                                           .chainApiFromDb()
                                           .flatMap(_.getBlockCount())
                                           .map(_ == ExpectedCount)
                                       },
                                       interval = 1000.millis)

        def hasFilterHeadersF =
          RpcUtil.retryUntilSatisfiedF(conditionF = () => {
                                         node
                                           .chainApiFromDb()
                                           .flatMap(_.getFilterHeaderCount())
                                           .map(_ == ExpectedCount)
                                       },
                                       interval = 1000.millis)

        def hasFiltersF =
          RpcUtil.retryUntilSatisfiedF(conditionF = () => {
                                         node
                                           .chainApiFromDb()
                                           .flatMap(_.getFilterCount())
                                           .map(_ == ExpectedCount)
                                       },
                                       interval = 1000.millis)

        for {
          _ <- hasBlocksF
          _ <- hasFilterHeadersF
          _ <- hasFiltersF
        } yield {
          val isCancelled = cancellable.cancel()
          if (!isCancelled) {
            logger.warn(s"Failed to cancel generating blocks on bitcoind")
          }
          succeed
        }
      }
  }

}
