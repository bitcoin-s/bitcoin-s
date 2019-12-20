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
import org.bitcoins.core.hd.HDPurpose

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
        purpose: HDPurpose,
        chain: HDChainType,
        acctIndex: Int
    ): Future[AddressDb] = {
      val recentOptFut: Future[Option[AddressDb]] = chain match {
        case Change =>
          wallet.addressDAO.findMostRecentChange(purpose, acctIndex)
        case External =>
          wallet.addressDAO.findMostRecentExternal(purpose, acctIndex)
      }

      recentOptFut.map {
        case Some(addr) => addr
        case None       => fail(s"Did not find $chain address!")
      }
    }

    def assertIndexIs(
        purpose: HDPurpose,
        chain: HDChainType,
        addrIndex: Int,
        accountIndex: Int): Future[Assertion] = {
      getMostRecent(purpose, chain, accountIndex) map { addr =>
        assert(addr.path.address.index == addrIndex)
      }
    }

    val addressesToGenerate = 10
    val addrRange = 0 to addressesToGenerate

    /**
      * Generate some addresses, and verify that the correct address index is
      * being reported
      */
    def testChain(
        purpose: HDPurpose,
        chain: HDChainType,
        accIdx: Int): Future[Assertion] = {
      val getAddrFunc: () => Future[BitcoinAddress] = chain match {
        case Change   => wallet.getNewChangeAddress _
        case External => wallet.getNewAddress _
      }
      for {
        _ <- {
          val addrF = chain match {
            case Change =>
              wallet.addressDAO.findMostRecentChange(purpose, accIdx)
            case External =>
              wallet.addressDAO.findMostRecentExternal(purpose, accIdx)
          }
          addrF.map {
            case Some(addr) =>
              fail(
                s"Found ${addr.address} $chain address although there was no previous addreses generated")
            case None =>
          }
        }
        _ <- FutureUtil.sequentially(addrRange)(_ => getAddrFunc())
        _ <- assertIndexIs(purpose,
                           chain,
                           accountIndex = accIdx,
                           addrIndex = addressesToGenerate)
        newest <- getAddrFunc()
        res <- getMostRecent(purpose, chain, accIdx).map { found =>
          assert(found.address == newest)
          assert(found.path.address.index == addressesToGenerate + 1)
        }
      } yield res
    }

    for {
      account <- wallet.getDefaultAccount()
      accIdx = account.hdAccount.index
      _ <- testChain(wallet.DEFAULT_HD_PURPOSE, External, accIdx)
      res <- testChain(wallet.DEFAULT_HD_PURPOSE, Change, accIdx)
    } yield res
  }

  it should "fail to unlock the wallet with a bad password" in {
    wallet: UnlockedWalletApi =>
      val badpassphrase = AesPassword.fromNonEmptyString("bad")
      val locked = wallet.lock()
      wallet.unlock(badpassphrase) match {
        case MnemonicNotFound          => fail(MnemonicNotFound)
        case BadPassword               => succeed
        case JsonParsingError(message) => fail(message)
        case UnlockWalletSuccess(_) =>
          fail("Unlocked wallet with bad password!")
      }
  }

  it should "match block filters" in { wallet: UnlockedWalletApi =>
    for {
      matched <- wallet.getMatchingBlocks(
        scripts = Vector(
          // this is a random address which is included into the test block
          BitcoinAddress("n1RH2x3b3ah4TGQtgrmNAHfmad9wr8U2QY").get.scriptPubKey),
        startOpt = None,
        endOpt = None
      )(system.dispatcher)
    } yield {
      assert(Vector(testBlockHash) == matched)
    }
  }

}
