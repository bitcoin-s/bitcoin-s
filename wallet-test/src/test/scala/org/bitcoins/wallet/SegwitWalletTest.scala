package org.bitcoins.wallet

import org.bitcoins.core.hd.HDPurposes
import org.bitcoins.core.protocol.Bech32Address
import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.bitcoins.wallet.api.UnlockedWalletApi
import org.scalatest.FutureOutcome

class SegwitWalletTest extends BitcoinSWalletTest {

  override type FixtureParam = UnlockedWalletApi

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    withSegwitWallet(test)
  }

  it should "generate segwit addresses" in { wallet: UnlockedWalletApi =>
    for {
      addr <- wallet.getNewAddress()
      account <- wallet.getDefaultAccount()
      otherAddr <- wallet.getNewAddress()
      allAddrs <- wallet.listAddresses()
    } yield {
      assert(account.hdAccount.purpose == HDPurposes.SegWit)
      assert(allAddrs.forall(_.address.isInstanceOf[Bech32Address]))
      assert(allAddrs.length == 2)
      assert(allAddrs.exists(_.address == addr))
      assert(allAddrs.exists(_.address == otherAddr))
    }
  }
}
