package org.bitcoins.wallet

import org.bitcoins.testkit.wallet.{BitcoinSWalletTest, WalletTestUtil}
import org.scalatest.FutureOutcome

class AddressHandlingTest extends BitcoinSWalletTest {
  type FixtureParam = Wallet

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    withNewWallet2Accounts(test)
  }

  behavior of "AddressHandling"

  it must "generate a new address for the default account and then find it" in { wallet: Wallet =>
    val addressF = wallet.getNewAddress()

    for {
      address <- addressF
      exists <- wallet.contains(address, None)
    } yield {
      assert(exists, s"Wallet must contain address after generating it")
    }
  }

  it must "generate an address for a non default account and then find it" in { wallet: Wallet =>
    val account1 = WalletTestUtil.getHdAccount1(wallet.walletConfig)
    val addressF = wallet.getNewAddress(account1)
    for {
      address <- addressF
      listAddressesForAcct <- wallet.listAddresses(account1)
      exists <- wallet.contains(address, Some(account1))
      doesNotExist <- wallet.contains(address, None)
    } yield {
      assert(listAddressesForAcct.nonEmpty)
      assert(listAddressesForAcct.map(_.address).contains(address))
      assert(exists, s"Wallet must contain address in specific after generating it")
      assert(doesNotExist, s"Wallet must NOT contain address in default account when address is specified")
    }
  }

}
