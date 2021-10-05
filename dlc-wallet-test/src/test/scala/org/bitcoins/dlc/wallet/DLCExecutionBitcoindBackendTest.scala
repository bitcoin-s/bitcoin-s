package org.bitcoins.dlc.wallet

import org.bitcoins.core.protocol.dlc.models.DLCState
import org.bitcoins.testkit.rpc.CachedBitcoindNewest
import org.bitcoins.testkit.wallet.{BitcoinSDualWalletTest, DLCWalletUtil}
import org.bitcoins.testkit.wallet.DLCWalletUtil.InitializedDLCWallet
import org.scalatest.FutureOutcome

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
      for {
        dlcAs <- dlcA.listDLCs()
        dlcBs <- dlcB.listDLCs()
      } yield {
        assert(dlcAs.length == 1)
        assert(dlcAs.head.state == DLCState.Broadcasted)

        assert(dlcBs.length == 1)
        assert(dlcBs.head.state == DLCState.Broadcasted)
      }
  }
}
