package org.bitcoins.wallet

import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.bitcoins.testkit.Implicits._
import org.scalatest.FutureOutcome
import org.bitcoins.testkit.fixtures.EmptyFixture
import org.bitcoins.testkit.core.gen.CryptoGenerators
import org.bitcoins.core.crypto.MnemonicCode
import org.bitcoins.core.crypto.AesPassword
import scala.util.Success
import java.nio.file.Files
import akka.compat.Future
import akka.compat.Future
import scala.concurrent.Future
import scala.collection.JavaConverters._
import java.nio.file.Path
import org.scalatest.BeforeAndAfterEach
import java.nio.file.Paths
import org.bitcoins.wallet.ReadMnemonicError.DecryptionError
import java.{util => ju}
import org.bitcoins.wallet.ReadMnemonicError.JsonParsingError
import org.bitcoins.server.BitcoinSAppConfig._
import scala.concurrent.Await
import scala.concurrent.duration._

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

  val passphrase = AesPassword.fromNonEmptyString("this_is_secret")
  val badPassphrase = AesPassword.fromNonEmptyString("this_is_also_secret")

  def getAndWriteMnemonic(): MnemonicCode = {
    val mnemonic = CryptoGenerators.mnemonicCode.sampleSome
    val encrypted = EncryptedMnemonicHelper.encrypt(mnemonic, passphrase)

    val _ =
      WalletStorage.writeMnemonicToDisk(encrypted)

    mnemonic
  }

  it must "write and read a mnemonic to disk" in { _ =>
    // no seed  should be in place at start of test
    assert(!WalletStorage.seedExists())

    val writtenMnemonic = getAndWriteMnemonic()

    // should have been written by now
    assert(WalletStorage.seedExists())

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
