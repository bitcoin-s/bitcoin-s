package org.bitcoins.node

import org.bitcoins.core.currency._
import org.bitcoins.chain.blockchain.ChainHandler
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.models.BlockHeaderDAO
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.models.Peer
import org.scalatest.FutureOutcome
import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.bitcoins.testkit.BitcoinSAppConfig
import org.bitcoins.wallet.config.WalletAppConfig

import scala.concurrent.Future
import org.bitcoins.node.networking.peer.DataMessageHandler
import scala.concurrent.Promise
import scala.concurrent.duration._
import org.scalatest.compatible.Assertion
import org.scalatest.exceptions.TestFailedException
import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.rpc.util.AsyncUtil

class NodeWithWalletTest extends BitcoinSWalletTest {

  override type FixtureParam = WalletWithBitcoind

  def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withNewWalletAndBitcoind(test)

  it must "load a bloom filter and receive information about received payments" in {
    param =>
      val WalletWithBitcoind(wallet, rpc) = param

      val chainHandler = {
        val bhDao = BlockHeaderDAO(config)
        ChainHandler(bhDao, config)
      }

      var txidFromBitcoind: Option[DoubleSha256Digest] = None

      val completionP = Promise[Assertion]

      val peer = Peer.fromBitcoind(rpc.instance)

      val callbacks = SpvNodeCallbacks(
        onBlockReceived = { block =>
          logger.error(s"Received a block")
          // for some reason this isn't triggered?
          completionP.failure(
            new TestFailedException(
              s"Received a block! We are only expecting merkle blocks",
              failedCodeStackDepth = 0))

        },
        onMerkleBlockReceived =
          block => logger.info(s"Received merkle block: $block"),
        onTxReceived = { tx =>
          logger.info(s"Received TX: $tx")
          if (txidFromBitcoind.contains(tx.txId)) {
            completionP.success(succeed)
          }
        },
      )

      /**
        * This is not ideal, how do we get one implicit value (`config`)
        * to resolve to multiple implicit parameters?
        */
      implicit val nodeConfig: NodeAppConfig = config
      implicit val chainConfig: ChainAppConfig = config
      val spv =
        SpvNode(peer, chainHandler, callbacks = callbacks)

      logger.info(
        s"Bitcoind instance has datadir: ${rpc.instance.authCredentials.datadir}")

      for {
        _ <- config.initialize()
        _ = logger.info(s"Initialized")

        address <- wallet.getNewAddress()
        info <- wallet.getAddressInfo(address).map {
          case Some(info) => info
          case None       => fail(s"Didn't get address info")
        }
        _ <- spv.addPubKey(info.pubkey)
        _ = logger.info(s"Added pubkey from wallet to SPV node")

        started <- spv.start()
        _ <- spv.sync()
        bitcoindHash <- rpc.getBestBlockHash
        _ <- {
          val isSyncedF =
            () => spv.chainApi.getBestBlockHash.map(_ == bitcoindHash)
          AsyncUtil.awaitConditionF(isSyncedF)
        }
        _ = logger.info(s"SPV node is synced")

        bloom = started.bloomFilter match {
          case Some(b) => b
          case None    => fail(s"Wallet had no bloom filter")
        }

        txid <- rpc.sendToAddress(address, 1.bitcoin).map { tx =>
          txidFromBitcoind = Some(tx.flip)
          val delay = 30.seconds
          val runnable: Runnable = { () =>
            val msg =
              s"Did not receive sent transaction within $delay"
            logger.error(msg)
            completionP.failure(new TestFailedException(msg, 0))
          }

          actorSystem.scheduler.scheduleOnce(delay, runnable)
          tx
        }
        _ = logger.info(s"bitcoind sent TX $txid")

        rpcAddress <- rpc.getNewAddress
        block +: _ <- rpc.generateToAddress(blocks = 1, rpcAddress)
        _ = logger.info(s"Generated a block")
        txInfo <- rpc.getTransaction(txid)
        _ = assert(txInfo.confirmations > 0 && txInfo.blockhash.contains(block))

        assertion <- completionP.future
      } yield assertion

  }
}
