package org.bitcoins.wallet

import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.bitcoins.testkit.wallet.FundWalletUtil.FundedWallet
import org.scalatest.FutureOutcome

class NeutrinoWalletTest extends BitcoinSWalletTest {

  override type FixtureParam = FundedWallet

  override def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withFundedWallet(test)

  behavior of "Wallet"

  it should "correctly send to an address" in { fundedWallet =>
    null
  }
}
