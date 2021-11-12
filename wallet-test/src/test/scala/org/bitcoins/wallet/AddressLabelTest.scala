package org.bitcoins.wallet

import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.bitcoins.testkit.wallet.FundWalletUtil.FundedWallet
import org.scalatest.FutureOutcome

class AddressLabelTest extends BitcoinSWalletTest {
  type FixtureParam = FundedWallet

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    withFundedWallet(test, getBIP39PasswordOpt())(getFreshWalletAppConfig)
  }

  behavior of "Address tags"

  it must "add two labels to the database" in { fundedWallet =>
    val wallet = fundedWallet.wallet
    
  }
}
