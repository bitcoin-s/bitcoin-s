package org.bitcoins.wallet

import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.testkit.wallet.FundWalletUtil.FundedWallet
import org.bitcoins.testkit.wallet.{BitcoinSWalletTest, WalletTestUtil}
import org.scalatest.FutureOutcome

class AddressHandlingTest extends BitcoinSWalletTest {
  type FixtureParam = FundedWallet

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    withFundedWallet(test)
  }

  behavior of "AddressHandling"

  it must "generate a new address for the default account and then find it" in {
    fundedWallet: FundedWallet =>
      val wallet = fundedWallet.wallet
      val addressF = wallet.getNewAddress()

      for {
        address <- addressF
        exists <- wallet.contains(address, None)
      } yield {
        assert(exists, s"Wallet must contain address after generating it")
      }
  }

  it must "generate an address for a non default account and then find it" in {
    fundedWallet: FundedWallet =>
      val wallet = fundedWallet.wallet
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
        assert(exists,
               s"Wallet must contain address in specific after generating it")
        assert(
          doesNotExist,
          s"Wallet must NOT contain address in default account when address is specified")
      }
  }

  it must "generate the same unused address until it receives funds" in {
    fundedWallet: FundedWallet =>
      val wallet = fundedWallet.wallet

      for {
        address1 <- wallet.getUnusedAddress
        exists <- wallet.contains(address1, None)
        _ = assert(exists, s"Wallet must contain address after generating it")
        address2 <- wallet.getUnusedAddress
        _ = assert(address1 == address2, "Must generate same address")
        _ <- wallet.sendToAddress(address1,
                                  Satoshis(10000),
                                  SatoshisPerVirtualByte.one)
        address3 <- wallet.getUnusedAddress
      } yield {
        assert(address1 != address3, "Must generate a new address")
        assert(address2 != address3, "Must generate a new address")
      }
  }
}
