package org.bitcoins.wallet

import java.nio.file.{Files, Path}

import org.bitcoins.core.crypto.{AesPassword, MnemonicCode}
import org.bitcoins.testkit.BitcoinSAppConfig._
import org.bitcoins.testkit.core.gen.CryptoGenerators
import org.bitcoins.testkit.fixtures.EmptyFixture
import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.bitcoins.wallet.ReadMnemonicError.{DecryptionError, JsonParsingError}
import org.scalatest.BeforeAndAfterEach

import scala.collection.JavaConverters._
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Success

class WalletStorageTest
    extends BitcoinSWalletTest
    with BeforeAndAfterEach
    with EmptyFixture {

  val datadir = config.walletConf.datadir

  override def beforeEach(): Unit = {
    // make sure datadir is created for reading/writing mnemonics
    Await.result(config.walletConf.initialize(), 5.seconds)

    Files
      .walk(datadir)
      .iterator()
      .asScala
      .filter((p: Path) => p != datadir)
      .foreach(Files.delete(_))
  }

  behavior of "WalletStorage"

  val passphrase = AesPassword("this_is_secret")
  val badPassphrase = AesPassword("this_is_also_secret")

  def getMnemonic: MnemonicCode =
    CryptoGenerators.mnemonicCode.sample.getOrElse(getMnemonic)

  def getAndWriteMnemonic(): MnemonicCode = {
    val mnemonic = getMnemonic
    val Success(encrypted) =
      EncryptedMnemonicHelper.encrypt(mnemonic, passphrase)

    val _ =
      WalletStorage.writeMnemonicToDisk(encrypted)

    mnemonic
  }

  it must "write and read a mnemonic to disk" in { _ =>
    val writtenMnemonic = getAndWriteMnemonic()
    val read =
      WalletStorage.decryptMnemonicFromDisk(passphrase)

    read match {
      case ReadMnemonicSuccess(readMnemonic) =>
        assert(writtenMnemonic == readMnemonic)
      case err: ReadMnemonicError => fail(err.toString)
    }
  }

  it must "fail to read a mnemonic with bad password" in { _ =>
    val writtenMnemonic = getAndWriteMnemonic()
    val read = WalletStorage.decryptMnemonicFromDisk(badPassphrase)

    read match {
      case ReadMnemonicSuccess(mnemonic) =>
        fail("Wrote and read with different passwords")
      case DecryptionError        => succeed
      case err: ReadMnemonicError => fail(err.toString)
    }
  }

  it must "fail to read a mnemonic that has bad JSON in it" in { _ =>
    val badJson =
      """
      | {
      |   "iv":"ba7722683dad8067df8d069ee04530cc",
      |   "cipherText":,
      |   "salt":"2b7e7d718139518070a87fbbda03ea33cdcda83b555020e9344774e6e7d08af2"
      | }
    """.stripMargin
    Files.write(datadir.resolve(WalletStorage.ENCRYPTED_SEED_FILE_NAME),
                badJson.getBytes())

    val read =
      WalletStorage.decryptMnemonicFromDisk(passphrase)

    read match {
      case JsonParsingError(_)     => succeed
      case res: ReadMnemonicResult => fail(res.toString())
    }
  }
}
