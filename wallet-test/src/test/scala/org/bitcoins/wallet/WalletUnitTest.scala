package org.bitcoins.wallet

import org.bitcoins.wallet.api.UnlockedWalletApi
import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.scalatest.FutureOutcome
import org.bitcoins.wallet.api.UnlockWalletError.BadPassword
import org.bitcoins.wallet.api.UnlockWalletError.JsonParsingError
import org.bitcoins.wallet.api.UnlockWalletSuccess
import org.bitcoins.core.crypto.AesPassword
import org.bitcoins.wallet.api.UnlockWalletError.MnemonicNotFound
import scala.concurrent.Future
import org.bitcoins.core.util.FutureUtil
import org.scalatest.compatible.Assertion
import org.bitcoins.core.hd.HDChainType
import org.bitcoins.core.hd.HDChainType.Change
import org.bitcoins.core.hd.HDChainType.External
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.wallet.models.AddressDb
import org.bitcoins.core.hd.HDChain

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

  it should "know what the last address index is" in { walletApi =>
    val wallet = walletApi.asInstanceOf[Wallet]

    def getMostRecent(
        chain: HDChainType,
        acctIndex: Int
    ): Future[AddressDb] = {
      val recentOptFut: Future[Option[AddressDb]] = chain match {
        case Change   => wallet.addressDAO.findMostRecentChange(acctIndex)
        case External => wallet.addressDAO.findMostRecentExternal(acctIndex)
      }

      recentOptFut.map {
        case Some(addr) => addr
        case None       => fail(s"Did not find $chain address!")
      }
    }

    def assertIndexIs(
        chain: HDChainType,
        addrIndex: Int,
        accountIndex: Int): Future[Assertion] = {
      getMostRecent(chain, accountIndex) map { addr =>
        assert(addr.path.address.index == addrIndex)
      }
    }

    val addressesToGenerate = 10
    val addrRange = 0 to addressesToGenerate

    /**
      * Generate some addresse, and verify that the correct address index is
      * being reported
      */
    def testChain(chain: HDChainType, accIdx: Int): Future[Assertion] = {
      val getAddrFunc: () => Future[BitcoinAddress] = chain match {
        case Change   => wallet.getNewChangeAddress _
        case External => wallet.getNewAddress _
      }
      for {
        _ <- {
          val addrF = chain match {
            case Change   => wallet.addressDAO.findMostRecentChange(accIdx)
            case External => wallet.addressDAO.findMostRecentExternal(accIdx)
          }
          addrF.map {
            case Some(addr) =>
              fail(
                s"Found ${addr.address} $chain address although there was no previous addreses generated")
            case None =>
          }
        }
        _ <- FutureUtil.sequentially(addrRange)(_ => getAddrFunc())
        _ <- assertIndexIs(chain,
                           accountIndex = accIdx,
                           addrIndex = addressesToGenerate)
        newest <- getAddrFunc()
        res <- getMostRecent(chain, accIdx).map { found =>
          assert(found.address == newest)
          assert(found.path.address.index == addressesToGenerate + 1)
        }
      } yield res
    }

    for {
      account <- wallet.getDefaultAccount()
      accIdx = account.hdAccount.index
      _ <- testChain(External, accIdx)
      res <- testChain(Change, accIdx)
    } yield res
  }

  it should "generate a bloom filter" in { walletApi: UnlockedWalletApi =>
    val wallet = walletApi.asInstanceOf[Wallet]
    for {
      _ <- FutureUtil.sequentially(0 until 10)(_ => wallet.getNewAddress())
      bloom <- wallet.getBloomFilter()
      pubkeys <- wallet.listPubkeys()
    } yield {
      pubkeys.foldLeft(succeed) { (_, pub) =>
        assert(bloom.contains(pub))
      }
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
