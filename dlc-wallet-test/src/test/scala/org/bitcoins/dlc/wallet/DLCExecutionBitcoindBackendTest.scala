package org.bitcoins.dlc.wallet

import org.bitcoins.core.protocol.dlc.models.{
  DLCState,
  DLCStatus,
  DisjointUnionContractInfo,
  SingleContractInfo
}
import org.bitcoins.testkit.rpc.CachedBitcoindNewest
import org.bitcoins.testkit.wallet.{BitcoinSDualWalletTest, DLCWalletUtil}
import org.bitcoins.testkit.wallet.DLCWalletUtil.InitializedDLCWallet
import org.scalatest.FutureOutcome

import scala.concurrent.Future

class DLCExecutionBitcoindBackendTest
    extends BitcoinSDualWalletTest
    with CachedBitcoindNewest {

  override type FixtureParam = (InitializedDLCWallet, InitializedDLCWallet)

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val outcomeF = for {
      bitcoind <- cachedBitcoindWithFundsF
      outcome = withDualDLCWallets(test = test,
                                   contractOraclePair =
                                     DLCWalletUtil.sampleContractOraclePair,
                                   bitcoind = bitcoind)
      fut <- outcome.toFuture
    } yield fut

    new FutureOutcome(outcomeF)
  }

  behavior of "DLCExecutionBitcoindBackendTest"

  it must "be able to broadcast a CET when the funding tx is unconfirmed" in {
    wallets =>
      val dlcA = wallets._1.wallet
      val dlcB = wallets._2.wallet
      val broadcastBF: Future[DLCStatus.Broadcasted] = for {
        dlcAs <- dlcA.listDLCs()
        dlcBs <- dlcB.listDLCs()
      } yield {
        assert(dlcAs.length == 1)
        assert(dlcAs.head.state == DLCState.Broadcasted)
        assert(dlcBs.length == 1)
        assert(dlcBs.head.state == DLCState.Broadcasted)
        dlcBs.head.asInstanceOf[DLCStatus.Broadcasted]
      }

      val isFundingTxUnconfirmedF = for {
        broadcastB <- broadcastBF
        bitcoind <- cachedBitcoindWithFundsF
        result <- bitcoind.getRawTransaction(broadcastB.fundingTxId)
      } yield {
        //make sure no confirmations on the funding tx
        assert(result.confirmations.isEmpty)
      }

      val executedF = for {
        broadcastB <- broadcastBF
        bitcoind <- cachedBitcoindWithFundsF
        _ <- isFundingTxUnconfirmedF
        contractInfo = broadcastB.contractInfo
        contractId = broadcastB.contractId
        dlcId = broadcastB.dlcId
        (oracleSigs, _) = {
          contractInfo match {
            case single: SingleContractInfo =>
              DLCWalletUtil.getSigs(single)
            case disjoint: DisjointUnionContractInfo =>
              sys.error(
                s"Cannot retrieve sigs for disjoint union contract, got=$disjoint")
          }
        }
        closingTx <- dlcB.executeDLC(contractId, oracleSigs)
        //broadcast the closing tx
        _ <- dlcB.broadcastTransaction(closingTx)
        dlcs <- dlcB
          .listDLCs()
          .map(_.filter(_.dlcId == dlcId))
        _ = assert(dlcs.length == 1)
        dlc = dlcs.head
        _ = assert(dlc.state == DLCState.Claimed)
        claimed = dlc.asInstanceOf[DLCStatus.Claimed]

        //make sure funding tx still doesn't have confs
        fundingTxResult <- bitcoind.getRawTransaction(claimed.fundingTxId)
        //make sure bitcoind sees it
        closingTxResult <- bitcoind.getRawTransaction(claimed.closingTxId)
      } yield {
        assert(fundingTxResult.confirmations.isEmpty)
        assert(closingTxResult.confirmations.isEmpty)
      }
      executedF
  }
}
