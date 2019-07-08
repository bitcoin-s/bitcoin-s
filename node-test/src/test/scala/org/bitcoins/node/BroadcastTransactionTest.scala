package org.bitcoins.node

import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.scalatest.FutureOutcome
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.node.models.Peer
import org.bitcoins.chain.models.BlockHeaderDAO
import org.bitcoins.chain.blockchain.ChainHandler
import org.bitcoins.testkit.node.NodeTestUtil
import org.bitcoins.core.currency._
import org.bitcoins.core.wallet.fee.SatoshisPerByte
import org.bitcoins.rpc.util.AsyncUtil
import org.bitcoins.core.protocol.transaction.Transaction
import org.scalactic.Bool
import scala.concurrent.Future
import scala.concurrent.duration._
import org.bitcoins.testkit.async.TestAsyncUtil

class BroadcastTransactionTest extends BitcoinSWalletTest {

  override type FixtureParam = WalletWithBitcoind

  def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withFundedWalletAndBitcoind(test)

  it must "broadcast a transaction" in { param =>
    val WalletWithBitcoind(wallet, rpc) = param

    /**
      * This is not ideal, how do we get one implicit value (`config`)
      * to resolve to multiple implicit parameters?
      */
    implicit val nodeConfig: NodeAppConfig = config
    implicit val chainConfig: ChainAppConfig = config

    def hasSeenTx(transaction: Transaction): Future[Boolean] = {
      rpc
        .getRawTransaction(transaction.txIdBE)
        .map { _ =>
          true
        }
        .recover {
          case err: Throwable
              if err.getMessage.contains(
                s"No such mempool or blockchain transaction") =>
            false
          case other =>
            logger.info(s"Received other error: $other")
            throw other
        }
    }

    for {
      _ <- config.initialize()

      address <- rpc.getNewAddress
      bloom <- wallet.getBloomFilter()

      spv <- {
        val peer = Peer.fromBitcoind(rpc.instance)
        val chainHandler = {
          val bhDao = BlockHeaderDAO()
          ChainHandler(bhDao, config)
        }

        val spv =
          SpvNode(peer, chainHandler, bloomFilter = bloom)
        spv.start()
      }
      _ <- spv.sync()
      _ <- NodeTestUtil.awaitSync(spv, rpc)

      tx <- wallet
        .sendToAddress(address, 1.bitcoin, SatoshisPerByte(10.sats))

      _ = spv.broadcastTransaction(tx)
      _ <- Future { Thread.sleep(5.seconds.toMillis) }
      _ <- TestAsyncUtil.awaitConditionF(() => hasSeenTx(tx),
                                         duration = 500.millis)
      fromBitcoind <- rpc.getRawTransaction(tx.txIdBE)

    } yield {
      assert(fromBitcoind.vout.exists(_.value == 1.bitcoin))
    }

  }
}
