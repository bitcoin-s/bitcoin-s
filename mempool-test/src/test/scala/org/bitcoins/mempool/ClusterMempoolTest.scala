package org.bitcoins.mempool

import org.bitcoins.core.api.mempool.MempoolAcceptResult
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.mempool.config.MempoolAppConfig
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.node.{NodeTestWithCachedBitcoindPair}
import org.bitcoins.testkit.node.fixture.NeutrinoNodeConnectedWithBitcoinds
import org.bitcoins.testkit.util.TorUtil
import org.scalatest.{FutureOutcome, Outcome}
import scala.concurrent.Future

class ClusterMempoolTest extends NodeTestWithCachedBitcoindPair {

  override protected def getFreshConfig: BitcoinSAppConfig = {
    BitcoinSTestAppConfig.getMultiPeerNeutrinoWithEmbeddedDbTestConfig(
      () => pgUrl(),
      Vector.empty
    )
  }

  override type FixtureParam = NeutrinoNodeConnectedWithBitcoinds

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val torClientF = if (TorUtil.torEnabled) torF else Future.unit

    val outcomeF: Future[Outcome] = for {
      _ <- torClientF
      bitcoinds <- clientsF
      outcome = withUnsyncedNeutrinoNodeConnectedToBitcoinds(
        test,
        bitcoinds.toVector
      )(system, getFreshConfig)
      f <- outcome.toFuture
    } yield f
    new FutureOutcome(outcomeF)
  }

  it must "fail to add a transaction to the mempool if we do not have its outpoint" in {
    case nodeConnectedWithBitcoinds =>
      val node = nodeConnectedWithBitcoinds.node
      val bitcoind = nodeConnectedWithBitcoinds.bitcoinds.head
      val addrF = bitcoind.getNewAddress
      val txIdF =
        addrF.flatMap(addr => bitcoind.sendToAddress(addr, Bitcoins.one))
      implicit val mempoolAppConfig =
        MempoolAppConfig(node.nodeAppConfig.baseDatadir, Vector.empty)
      val mempool = ClusterMempool(Map.empty)
      for {
        txId <- txIdF
        tx <- bitcoind.getRawTransactionRaw(txId)
        result <- mempool.acceptToMemoryPool(tx)
      } yield {
        assert(result.isInstanceOf[MempoolAcceptResult.Invalid])
      }
  }
}
