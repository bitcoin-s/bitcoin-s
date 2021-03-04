package org.bitcoins.wallet

import org.bitcoins.core.api.wallet.NeutrinoWalletApi.BlockMatchingResponse
import org.bitcoins.core.api.wallet.db.{AddressDb, TransactionDbHelper}
import org.bitcoins.core.hd.HDChainType.{Change, External}
import org.bitcoins.core.hd.{AddressType, HDAccount, HDChainType}
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.script.{
  MultiSignatureScriptPubKey,
  P2PKHScriptPubKey,
  P2SHScriptPubKey,
  P2WPKHWitnessSPKV0
}
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.core.wallet.keymanagement.KeyManagerUnlockError
import org.bitcoins.core.wallet.keymanagement.KeyManagerUnlockError.MnemonicNotFound
import org.bitcoins.crypto.{AesPassword, ECPublicKey}
import org.bitcoins.testkitcore.util.TransactionTestUtil._
import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.scalatest.FutureOutcome
import org.scalatest.compatible.Assertion

import java.nio.file.Files
import scala.concurrent.Future

class WalletUnitTest extends BitcoinSWalletTest {
  private val bip39PasswordOpt: Option[String] = getBIP39PasswordOpt()

  override type FixtureParam = Wallet

  override def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withNewWallet(test, bip39PasswordOpt)

  behavior of "Wallet - unit test"

  it must "write the mnemonic seed in the correct directory" in {
    wallet: Wallet =>
      assert(Files.exists(wallet.walletConfig.seedPath))
  }

  it should "create a new wallet" in { wallet: Wallet =>
    for {
      accounts <- wallet.listAccounts()
      addresses <- wallet.listAddresses()
    } yield {
      assert(accounts.length == 3) // legacy, segwit and nested segwit
      assert(addresses.isEmpty)
    }
  }

  it should "generate addresses" in { wallet: Wallet =>
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

  it should "know what the last address index is" in { wallet =>
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

    /** Generate some addresses, and verify that the correct address index is
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

  it should "fail to unlock the wallet with a bad aes password" in {
    wallet: Wallet =>
      val badPassphrase = Some(AesPassword.fromNonEmptyString("bad"))

      val errorType = wallet.unlock(badPassphrase, None) match {
        case Right(_)  => fail("Unlocked wallet with bad password!")
        case Left(err) => err
      }
      errorType match {
        case KeyManagerUnlockError.MnemonicNotFound          => fail(MnemonicNotFound)
        case KeyManagerUnlockError.BadPassword               => succeed
        case KeyManagerUnlockError.JsonParsingError(message) => fail(message)
      }
  }

  it should "match block filters" in { wallet: Wallet =>
    for {
      height <- wallet.chainQueryApi.getFilterCount()
      filtersResponse <- chainQueryApi.getFiltersBetweenHeights(startHeight = 0,
                                                                endHeight =
                                                                  height)
      matched <- wallet.findMatches(
        filters = filtersResponse,
        scripts = Vector(
          // this is a random address which is included into the test block
          BitcoinAddress("n1RH2x3b3ah4TGQtgrmNAHfmad9wr8U2QY").scriptPubKey),
        parallelismLevel = 1
      )
    } yield {
      assert(
        Vector(BlockMatchingResponse(blockHash = testBlockHash,
                                     blockHeight = 1)) == matched)
    }
  }

  it must "be able to call initialize twice without throwing an exception if we have the same key manager" in {
    wallet: Wallet =>
      val twiceF = Wallet.initialize(wallet, bip39PasswordOpt).flatMap { _ =>
        Wallet.initialize(wallet, bip39PasswordOpt)
      }

      twiceF.map(_ => succeed)

  }

  it must "be able to detect an incompatible key manager with a wallet" in {
    wallet: Wallet =>
      recoverToSucceededIf[RuntimeException] {
        Wallet.initialize(wallet, bip39PasswordOpt).flatMap { _ =>
          //use a BIP39 password to make the key-managers different
          Wallet.initialize(
            wallet,
            Some("random-password-to-make-key-managers-different"))
        }
      }
  }

  it must "be able to sign a psbt with a key path" in { wallet: Wallet =>
    val dummyKey = ECPublicKey.freshPublicKey

    for {
      accountDb <- wallet.accountDAO.findAll().map(_.head)
      addr <- wallet.getNewAddress(accountDb)
      addrDb <- wallet.addressDAO.findAddress(addr).map(_.get)
      walletKey = addrDb.ecPublicKey
      walletPath = addrDb.path

      spk = MultiSignatureScriptPubKey(2, Vector(dummyKey, walletKey))
      dummyPrevTx = dummyTx(spk = spk)
      prevTxDb = TransactionDbHelper.fromTransaction(dummyPrevTx)
      _ <- wallet.transactionDAO.create(prevTxDb)

      psbt = dummyPSBT(prevTxId = dummyPrevTx.txId)
        .addKeyPathToInput(accountDb.xpub, walletPath, walletKey, 0)

      signed <- wallet.signPSBT(psbt)
    } yield {
      assert(signed != psbt)
      assert(
        signed.inputMaps.head.partialSignatures.exists(_.pubKey == walletKey))
    }
  }

  it must "be able to sign a psbt with our own p2pkh utxo" in {
    wallet: Wallet =>
      for {
        addr <- wallet.getNewAddress(AddressType.Legacy)
        addrDb <- wallet.addressDAO.findAddress(addr).map(_.get)
        walletKey = addrDb.ecPublicKey

        spk = addr.scriptPubKey
        _ = assert(spk == P2PKHScriptPubKey(walletKey))
        dummyPrevTx = dummyTx(spk = spk)
        _ <- wallet.processTransaction(dummyPrevTx, blockHashOpt = None)

        psbt = dummyPSBT(prevTxId = dummyPrevTx.txId)

        signed <- wallet.signPSBT(psbt)
      } yield {
        assert(signed != psbt)
        assert(
          signed.inputMaps.head.partialSignatures.exists(_.pubKey == walletKey))
      }
  }

  it must "be able to sign a psbt with our own p2sh segwit utxo" in {
    wallet: Wallet =>
      for {
        addr <- wallet.getNewAddress(AddressType.NestedSegWit)
        addrDb <- wallet.addressDAO.findAddress(addr).map(_.get)
        walletKey = addrDb.ecPublicKey

        spk = addr.scriptPubKey
        _ = assert(spk == P2SHScriptPubKey(P2WPKHWitnessSPKV0(walletKey)))
        dummyPrevTx = dummyTx(spk = spk)
        _ <- wallet.processTransaction(dummyPrevTx, blockHashOpt = None)

        psbt = dummyPSBT(prevTxId = dummyPrevTx.txId)

        signed <- wallet.signPSBT(psbt)
      } yield {
        assert(signed != psbt)
        assert(
          signed.inputMaps.head.partialSignatures.exists(_.pubKey == walletKey))
      }
  }

  it must "be able to sign a psbt with our own p2wpkh utxo" in {
    wallet: Wallet =>
      for {
        addr <- wallet.getNewAddress(AddressType.SegWit)
        addrDb <- wallet.addressDAO.findAddress(addr).map(_.get)
        walletKey = addrDb.ecPublicKey

        spk = addr.scriptPubKey
        _ = assert(spk == P2WPKHWitnessSPKV0(walletKey))
        dummyPrevTx = dummyTx(spk = spk)
        _ <- wallet.processTransaction(dummyPrevTx, blockHashOpt = None)

        psbt = dummyPSBT(prevTxId = dummyPrevTx.txId)
          .addUTXOToInput(dummyPrevTx, 0)

        signed <- wallet.signPSBT(psbt)
      } yield {
        assert(signed != psbt)
        assert(
          signed.inputMaps.head.partialSignatures.exists(_.pubKey == walletKey))
      }
  }

  it must "be able to sign a psbt with no wallet utxos" in { wallet: Wallet =>
    val psbt = dummyPSBT()
    for {
      signed <- wallet.signPSBT(psbt)
    } yield assert(signed == psbt)
  }
}
