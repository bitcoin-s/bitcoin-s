package org.bitcoins.wallet

import org.bitcoins.wallet.api.UnlockedWalletApi
import org.bitcoins.wallet.util.BitcoinSWalletTest
import org.scalatest.FutureOutcome

class WalletUnitTest extends BitcoinSWalletTest {

  override type FixtureParam = UnlockedWalletApi

  override def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withNewWallet(test)

  behavior of "Wallet - unit test"

  it should "create a new wallet" in { wallet: UnlockedWalletApi =>
    for {
      accounts <- wallet.listAccounts()
      addresses <- wallet.listAddresses()
    } yield {
      assert(accounts.length == 1)
      assert(addresses.isEmpty)
    }
  }

  // eventually this test should NOT succeed, as BIP44
  // requires a limit to addresses being generated when
  // they haven't received any funds
  it should "generate addresses" in { wallet: UnlockedWalletApi =>
    for {
      addr <- wallet.getNewAddress()
      otherAddr <- wallet.getNewAddress()
      allAddrs <- wallet.listAddresses()
    } yield {
      assert(allAddrs.length == 2)
      assert(allAddrs.exists(_.address == addr))
      assert(allAddrs.exists(_.address == otherAddr))
    }
  }

}
