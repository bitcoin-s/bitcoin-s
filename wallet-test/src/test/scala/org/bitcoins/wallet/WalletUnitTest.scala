package org.bitcoins.wallet

import java.nio.file.Files

import org.bitcoins.core.hd.HDChainType.{Change, External}
import org.bitcoins.core.hd.{HDAccount, HDChainType, HDPurpose}
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.crypto.AesPassword
import org.bitcoins.keymanager.KeyManagerUnlockError.MnemonicNotFound
import org.bitcoins.keymanager.{KeyManagerUnlockError, WalletStorage}
import org.bitcoins.testkit.EmbeddedPg
import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.bitcoins.wallet.api.WalletApi.BlockMatchingResponse
import org.bitcoins.wallet.api.WalletApi
import org.bitcoins.wallet.models.AddressDb
import org.scalatest.FutureOutcome
import org.scalatest.compatible.Assertion

import scala.concurrent.Future

class WalletUnitTest extends BitcoinSWalletTest {

  override type FixtureParam = WalletApi

  override def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withNewWallet(test)

  behavior of "Wallet - unit test"

  it must "write the mnemonic seed to the root datadir -- NOT A NETWORK sub directory" in {
    wallet: WalletApi =>
      //since datadir has the path that relates it to a network ('mainnet'/'testnet'/'regtest')
      //we need to get the parent of that to find where the encrypted seed should be
      //this is where the bitcoin-s.conf should live too.
      val datadir = wallet.walletConfig.baseDatadir

      assert(
        Files.exists(datadir.resolve(WalletStorage.ENCRYPTED_SEED_FILE_NAME)))

  }

  it should "create a new wallet" in { wallet: WalletApi =>
    for {
      accounts <- wallet.listAccounts()
      addresses <- wallet.listAddresses()
    } yield {
      assert(accounts.length == 3) // legacy, segwit and nested segwit
      assert(addresses.isEmpty)
    }
  }

  it should "generate addresses" in { wallet: WalletApi =>
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
        hdAccount: HDAccount,
        chain: HDChainType): Future[AddressDb] = {
      val recentOptFut: Future[Option[AddressDb]] = chain match {
        case Change =>
          wallet.addressDAO.findMostRecentChange(hdAccount)
        case External =>
          wallet.addressDAO.findMostRecentExternal(hdAccount)
      }

      recentOptFut.map {
        case Some(addr) => addr
        case None       => fail(s"Did not find $chain address!")
      }
    }

    def assertIndexIs(
        hdAccount: HDAccount,
        chain: HDChainType,
        addrIndex: Int): Future[Assertion] = {
      getMostRecent(hdAccount, chain) map { addr =>
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
        hdAccount: HDAccount,
        chain: HDChainType): Future[Assertion] = {
      val getAddrFunc: () => Future[BitcoinAddress] = chain match {
        case Change   => wallet.getNewChangeAddress _
        case External => wallet.getNewAddress _
      }
      for {
        _ <- {
          val addrF = chain match {
            case Change =>
              wallet.addressDAO.findMostRecentChange(hdAccount)
            case External =>
              wallet.addressDAO.findMostRecentExternal(hdAccount)
          }
          addrF.map {
            case Some(addr) =>
              fail(
                s"Found ${addr.address} $chain address although there was no previous addreses generated")
            case None =>
          }
        }
        _ <- FutureUtil.sequentially(addrRange)(_ => getAddrFunc())
        _ <- assertIndexIs(hdAccount, chain, addrIndex = addressesToGenerate)
        newest <- getAddrFunc()
        res <- getMostRecent(hdAccount, chain).map { found =>
          assert(found.address == newest)
          assert(found.path.address.index == addressesToGenerate + 1)
        }
      } yield res
    }

    for {
      account <- wallet.getDefaultAccount()
      _ <- testChain(hdAccount = account.hdAccount, External)
      res <- testChain(hdAccount = account.hdAccount, Change)
    } yield res
  }

  it should "fail to unlock the wallet with a bad password" in {
    wallet: WalletApi =>
      val badpassphrase = AesPassword.fromNonEmptyString("bad")

      val errorType = wallet.unlock(badpassphrase, None) match {
        case Right(_)  => fail("Unlocked wallet with bad password!")
        case Left(err) => err
      }
      errorType match {
        case KeyManagerUnlockError.MnemonicNotFound          => fail(MnemonicNotFound)
        case KeyManagerUnlockError.BadPassword               => succeed
        case KeyManagerUnlockError.JsonParsingError(message) => fail(message)
      }
  }

  it should "match block filters" in { wallet: WalletApi =>
    for {
      matched <- wallet.getMatchingBlocks(
        scripts = Vector(
          // this is a random address which is included into the test block
          BitcoinAddress("n1RH2x3b3ah4TGQtgrmNAHfmad9wr8U2QY").get.scriptPubKey),
        startOpt = None,
        endOpt = None
      )(system.dispatcher)
    } yield {
      assert(
        Vector(BlockMatchingResponse(blockHash = testBlockHash,
                                     blockHeight = 1)) == matched)
    }
  }
}
