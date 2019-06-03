package org.bitcoins.wallet

import org.bitcoins.wallet.api.UnlockedWalletApi
import org.bitcoins.wallet.util.BitcoinSWalletTest
import org.scalatest.FutureOutcome
import org.bitcoins.wallet.api.UnlockWalletError.BadPassword
import org.bitcoins.wallet.api.UnlockWalletError.JsonParsingError
import org.bitcoins.wallet.api.UnlockWalletSuccess
import org.bitcoins.core.crypto.AesPassword
import org.bitcoins.wallet.api.UnlockWalletError.MnemonicNotFound
import com.typesafe.config.ConfigFactory
import org.bitcoins.core.protocol.P2PKHAddress
import org.bitcoins.core.protocol.Bech32Address

class SegwitWalletTest extends BitcoinSWalletTest {

  override type FixtureParam = UnlockedWalletApi

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val confOverride =
      ConfigFactory.parseString("bitcoin-s.wallet.defaultAccountType = segwit")
    val conf = config.walletConf.withOverrides(confOverride)
    withNewConfiguredWallet(confOverride)(test)
  }

  // eventually this test should NOT succeed, as BIP44
  // requires a limit to addresses being generated when
  // they haven't received any funds
  it should "generate segwit addresses" in { wallet: UnlockedWalletApi =>
    for {
      addr <- wallet.getNewAddress()
      otherAddr <- wallet.getNewAddress()
      allAddrs <- wallet.listAddresses()
    } yield {
      assert(allAddrs.forall(_.address.isInstanceOf[Bech32Address]))
      assert(allAddrs.length == 2)
      assert(allAddrs.exists(_.address == addr))
      assert(allAddrs.exists(_.address == otherAddr))
    }
  }
}
