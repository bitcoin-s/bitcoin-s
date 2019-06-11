package org.bitcoins.wallet

import org.bitcoins.wallet.api.UnlockedWalletApi
import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.scalatest.FutureOutcome
import org.bitcoins.wallet.api.UnlockWalletError.BadPassword
import org.bitcoins.wallet.api.UnlockWalletError.JsonParsingError
import org.bitcoins.wallet.api.UnlockWalletSuccess
import org.bitcoins.core.crypto.AesPassword
import org.bitcoins.wallet.api.UnlockWalletError.MnemonicNotFound
import com.typesafe.config.ConfigFactory
import org.bitcoins.core.protocol.P2PKHAddress
import org.bitcoins.core.hd.HDPurposes

class LegacyWalletTest extends BitcoinSWalletTest {

  override type FixtureParam = UnlockedWalletApi

  override def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withLegacyWallet(test)

  it should "generate legacy addresses" in { wallet: UnlockedWalletApi =>
    for {
      addr <- wallet.getNewAddress()
      account <- wallet.getDefaultAccount()
      otherAddr <- wallet.getNewAddress()
      allAddrs <- wallet.listAddresses()
    } yield {
      assert(account.hdAccount.purpose == HDPurposes.Legacy)
      assert(allAddrs.forall(_.address.isInstanceOf[P2PKHAddress]))
      assert(allAddrs.length == 2)
      assert(allAddrs.exists(_.address == addr))
      assert(allAddrs.exists(_.address == otherAddr))
    }
  }
}
