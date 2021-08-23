package org.bitcoins.node

import org.bitcoins.core.currency._
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.rpc.BitcoindException
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.async.TestAsyncUtil
import org.bitcoins.testkit.node.NodeTestWithCachedBitcoindNewest
import org.bitcoins.testkit.node.fixture.SpvNodeConnectedWithBitcoind
import org.bitcoins.testkit.util.TorUtil
import org.scalatest.{FutureOutcome, Outcome}

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.control.NonFatal

class BroadcastTransactionTest extends NodeTestWithCachedBitcoindNewest {

  /** Wallet config with data directory set to user temp directory */
  override protected def getFreshConfig: BitcoinSAppConfig =
    BitcoinSTestAppConfig.getSpvWithEmbeddedDbTestConfig(pgUrl, Vector.empty)

  override type FixtureParam = SpvNodeConnectedWithBitcoind

  def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val torClientF = if (TorUtil.torEnabled) torF else Future.unit

    val outcome: Future[Outcome] = for {
      _ <- torClientF
      bitcoind <- cachedBitcoindWithFundsF
      outcome = withSpvNodeConnectedToBitcoindCached(test, bitcoind)(
        system,
        getFreshConfig)
      f <- outcome.toFuture
    } yield f
    new FutureOutcome(outcome)
  }

  private val sendAmount = 1.bitcoin

  def createValidTx(rpc: BitcoindRpcClient): Future[Transaction] = {
    for {
      rawTx <-
        rpc.createRawTransaction(Vector.empty, Map(junkAddress -> sendAmount))
      fundedTx <- rpc.fundRawTransaction(rawTx)
      tx <- rpc.signRawTransactionWithWallet(fundedTx.hex).map(_.hex)
    } yield tx
  }

  it must "safely broadcast a transaction twice" in { param =>
    val SpvNodeConnectedWithBitcoind(node, rpc) = param

    for {
      tx <- createValidTx(rpc)

      _ <- node.broadcastTransaction(tx)
      _ <- node.broadcastTransaction(tx)

      txDbOpt <- node.txDAO.findByHash(tx.txId)
    } yield {
      assert(txDbOpt.isDefined)
      assert(txDbOpt.get.transaction == tx)
    }
  }

  it must "broadcast a transaction" in { param =>
    val SpvNodeConnectedWithBitcoind(node, rpc) = param

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

    def attemptBroadcast(tx: Transaction): Future[Unit] = {
      for {
        _ <- node.broadcastTransaction(tx)
        txOpt <- node.txDAO.findByHash(tx.txId)
        _ = assert(
          txOpt.isDefined,
          "Transaction was not added to BroadcastableTransaction database")
        _ <- TestAsyncUtil.awaitConditionF(() => hasSeenTx(tx),
                                           interval = 1.second,
                                           maxTries = 25)
      } yield ()
    }

    for {
      bitcoindBalancePreBroadcast <- rpc.getBalance

      tx <- createValidTx(rpc)

      _ <- attemptBroadcast(tx)
        .recoverWith { case NonFatal(_) =>
          attemptBroadcast(tx)
        }
      _ <- rpc.generateToAddress(blocks = 1, junkAddress)
      bitcoindBalancePostBroadcast <- rpc.getBalance

    } yield assert(
      // pre-balance - sent amount + 1 block reward maturing +/- fees
      (bitcoindBalancePreBroadcast - sendAmount + 50.bitcoins).satoshis.toLong === bitcoindBalancePostBroadcast.satoshis.toLong +- 5000)
  }
}
