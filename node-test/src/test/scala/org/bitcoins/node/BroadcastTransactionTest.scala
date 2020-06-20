package org.bitcoins.node

import org.bitcoins.core.currency._
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.rpc.BitcoindException
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.async.TestAsyncUtil
import org.bitcoins.testkit.node.{
  NodeTestUtil,
  NodeUnitTest,
  SpvNodeFundedWalletBitcoind
}
import org.scalatest.FutureOutcome

import scala.concurrent.Future
import scala.concurrent.duration._

class BroadcastTransactionTest extends NodeUnitTest {

  /** Wallet config with data directory set to user temp directory */
  implicit override protected def config: BitcoinSAppConfig =
    BitcoinSTestAppConfig.getSpvWithEmbeddedDbTestConfig(pgUrl)

  override type FixtureParam = SpvNodeFundedWalletBitcoind

  def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withSpvNodeFundedWalletBitcoind(test,
                                    NodeCallbacks.empty,
                                    getBIP39PasswordOpt())

  private val sendAmount = 1.bitcoin

  it must "broadcast a transaction" in { param =>
    val SpvNodeFundedWalletBitcoind(node, wallet, rpc, _) = param

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

    val addrF = rpc.getNewAddress
    val balanceF = rpc.getBalance

    for {
      _ <- wallet.getBloomFilter()
      _ <- node.sync()
      _ <- NodeTestUtil.awaitSync(node, rpc)

      address <- addrF
      tx <-
        wallet
          .sendToAddress(address, sendAmount, None)

      bitcoindBalancePreBroadcast <- balanceF
      _ <- node.broadcastTransaction(tx)
      _ <-
        TestAsyncUtil
          .awaitConditionF(() => hasSeenTx(tx),
                           duration = 1.second,
                           maxTries = 25)
          .recoverWith {
            case _: Throwable =>
              for {
                _ <- node.broadcastTransaction(tx)
                _ <- TestAsyncUtil.awaitConditionF(() => hasSeenTx(tx),
                                                   duration = 1.second,
                                                   maxTries = 25)
              } yield ()
          }
      _ <- rpc.generateToAddress(blocks = 1, junkAddress)
      bitcoindBalancePostBroadcast <- rpc.getBalance

    } yield assert(
      // pre-balance + sent amount + 1 block reward maturing
      bitcoindBalancePreBroadcast + sendAmount + 50.bitcoins == bitcoindBalancePostBroadcast)

  }
}
