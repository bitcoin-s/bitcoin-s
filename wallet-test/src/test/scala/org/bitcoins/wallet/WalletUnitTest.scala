package org.bitcoins.wallet

import org.bitcoins.wallet.api.UnlockedWalletApi
import org.bitcoins.wallet.util.BitcoinSWalletTest
import org.scalatest.FutureOutcome
import org.bitcoins.wallet.api.UnlockWalletError.BadPassword
import org.bitcoins.wallet.api.UnlockWalletError.JsonParsingError
import org.bitcoins.wallet.api.UnlockWalletSuccess
import org.bitcoins.core.crypto.AesPassword
import org.bitcoins.wallet.api.UnlockWalletError.MnemonicNotFound

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
      assert(accounts.length == 3) // legacy, segwit and nested segwit
      assert(addresses.isEmpty)
    }
  }

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

  it should "lock and unlock the wallet" in { wallet: UnlockedWalletApi =>
    val passphrase = wallet.passphrase
    val locked = wallet.lock()
    val unlocked = wallet.unlock(passphrase) match {
      case MnemonicNotFound                       => fail(MnemonicNotFound)
      case BadPassword                            => fail(BadPassword)
      case JsonParsingError(message)              => fail(message)
      case UnlockWalletSuccess(unlockedWalletApi) => unlockedWalletApi
    }

    assert(wallet.mnemonicCode == unlocked.mnemonicCode)
  }

  it should "fail to unlock the wallet with a bad password" in {
    wallet: UnlockedWalletApi =>
      val badpassphrase = AesPassword("bad")
      val locked = wallet.lock()
      wallet.unlock(badpassphrase) match {
        case MnemonicNotFound          => fail(MnemonicNotFound)
        case BadPassword               => succeed
        case JsonParsingError(message) => fail(message)
        case UnlockWalletSuccess(_) =>
          fail("Unlocked wallet with bad password!")
      }
  }

}
