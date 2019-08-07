package org.bitcoins.node

import org.bitcoins.core.currency._
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.wallet.fee.SatoshisPerByte
import org.bitcoins.rpc.BitcoindException
import org.bitcoins.testkit.async.TestAsyncUtil
import org.bitcoins.testkit.node.NodeUnitTest.SpvNodeFundedWalletBitcoind
import org.bitcoins.testkit.node.{NodeTestUtil, NodeUnitTest}
import org.scalatest.FutureOutcome

import scala.concurrent.Future
import scala.concurrent.duration._

class BroadcastTransactionTest extends NodeUnitTest {

  override type FixtureParam = SpvNodeFundedWalletBitcoind

  def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withSpvNodeFundedWalletBitcoind(test, SpvNodeCallbacks.empty)

  it must "broadcast a transaction" in { param =>
    val SpvNodeFundedWalletBitcoind(spv, wallet, rpc) = param

    def hasSeenTx(transaction: Transaction): Future[Boolean] = {
      rpc
        .getRawTransaction(transaction.txIdBE)
        .map { _ =>
          true
        }
        .recover {
          case BitcoindException.InvalidAddressOrKey(_) =>
            false
          case other =>
            logger.error(
              s"Received unexpected error on getrawtransaction: $other")
            throw other
        }
    }

    for {

      address <- rpc.getNewAddress
      bloom <- wallet.getBloomFilter()
      _ <- spv.sync()
      _ <- NodeTestUtil.awaitSync(spv, rpc)

      tx <- wallet
        .sendToAddress(address, 1.bitcoin, SatoshisPerByte(10.sats))

      bitcoindBalancePreBroadcast <- rpc.getBalance
      _ = spv.broadcastTransaction(tx)
      _ <- TestAsyncUtil.awaitConditionF(() => hasSeenTx(tx),
                                         duration = 1.second)
      fromBitcoind <- rpc.getRawTransaction(tx.txIdBE)
      _ = assert(fromBitcoind.vout.exists(_.value == 1.bitcoin))

      _ <- rpc.getNewAddress.flatMap(rpc.generateToAddress(1, _))
      bitcoindBalancePostBroadcast <- rpc.getBalance

    } yield assert(bitcoindBalancePreBroadcast < bitcoindBalancePostBroadcast)

  }
}
