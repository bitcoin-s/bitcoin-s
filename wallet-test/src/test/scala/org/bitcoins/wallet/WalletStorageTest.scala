package org.bitcoins.wallet

import org.bitcoins.wallet.util.BitcoinSWalletTest
import org.scalatest.FutureOutcome
import org.bitcoins.testkit.fixtures.EmptyFixture
import org.bitcoins.testkit.core.gen.CryptoGenerators
import javassist.bytecode.Mnemonic
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

class WalletStorageTest
    extends BitcoinSWalletTest
    with BeforeAndAfterEach
    with EmptyFixture {

  val datadir = appConfig.datadir

  override def beforeEach(): Unit = {
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
