package org.bitcoins.wallet

import org.bitcoins.core.currency.{Bitcoins, CurrencyUnits}
import org.bitcoins.core.protocol.transaction.TransactionOutput
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.testkit.util.{TestUtil, TransactionTestUtil}
import org.bitcoins.testkit.wallet.{BitcoinSWalletTest, WalletTestUtil}
import org.bitcoins.testkit.wallet.BitcoinSWalletTest.FundedWallet
import org.bitcoins.wallet.api.UnlockedWalletApi
import org.scalatest.FutureOutcome

class FundTransactionTest extends BitcoinSWalletTest {

  override type FixtureParam = FundedWallet
  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    withFundedWallet(test)
  }

  val destination = TransactionOutput(Bitcoins(0.5),TestUtil.p2pkhScriptPubKey)
  val feeRate = SatoshisPerVirtualByte.zero

  it must "fund a simple raw transaction that requires one utxo" in { fundedWallet : FundedWallet =>

    val wallet = fundedWallet.wallet
    wallet.fundRawTransaction(Vector(destination),
      feeRate,
      fromAccount = ???)
    fail()
  }

  it must "fail to fund a raw transaction" in { fundedWallet: FundedWallet =>
    val wallet = fundedWallet.wallet
    wallet.fundRawTransaction(Vector(destination),
      feeRate,
      fromAccount = ???)
    fail()
  }
}
