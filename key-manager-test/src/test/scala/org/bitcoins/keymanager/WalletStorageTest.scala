package org.bitcoins.keymanager

import java.nio.file.{Files, Path}
import java.util.NoSuchElementException

import org.bitcoins.core.crypto.BIP39Seed
import org.bitcoins.core.crypto.ExtKeyVersion.SegWitMainNetPriv
import org.bitcoins.core.util.TimeUtil
import org.bitcoins.crypto.AesPassword
import org.bitcoins.keymanager.ReadMnemonicError.{
  DecryptionError,
  JsonParsingError
}
import org.bitcoins.keymanager.bip39.BIP39KeyManager
import org.bitcoins.testkit.Implicits._
import org.bitcoins.testkit.core.gen.CryptoGenerators
import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.bitcoins.wallet.config.WalletAppConfig
import org.scalatest.{BeforeAndAfterEach, FutureOutcome}
import ujson.Value.InvalidData

class WalletStorageTest extends BitcoinSWalletTest with BeforeAndAfterEach {

  override type FixtureParam = WalletAppConfig

  override def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withWalletConfig(test)

  def getSeedPath(config: WalletAppConfig): Path = {
    config.seedPath
  }

  behavior of "WalletStorage"

  val passphrase = AesPassword.fromNonEmptyString("this_is_secret")
  val badPassphrase = AesPassword.fromNonEmptyString("this_is_also_secret")

  def getAndWriteMnemonic(walletConf: WalletAppConfig): DecryptedMnemonic = {
    val mnemonicCode = CryptoGenerators.mnemonicCode.sampleSome
    val decryptedMnemonic = DecryptedMnemonic(mnemonicCode, TimeUtil.now)
    val encrypted =
      EncryptedMnemonicHelper.encrypt(decryptedMnemonic, passphrase)
    val seedPath = getSeedPath(walletConf)
    val _ =
      WalletStorage.writeMnemonicToDisk(seedPath, encrypted)
    decryptedMnemonic
  }

  it must "write and read a mnemonic to disk" in {
    walletConf: WalletAppConfig =>
      assert(!walletConf.seedExists())

      val writtenMnemonic = getAndWriteMnemonic(walletConf)

      // should have been written by now
      assert(walletConf.seedExists())
      val seedPath = getSeedPath(walletConf)
      val read =
        WalletStorage.decryptMnemonicFromDisk(seedPath, passphrase)
      read match {
        case Right(readMnemonic) =>
          assert(writtenMnemonic.mnemonicCode == readMnemonic.mnemonicCode)
          // Need to compare using getEpochSecond because when reading an epoch second
          // it will not include the milliseconds that writtenMnemonic will have
          assert(
            writtenMnemonic.creationTime.getEpochSecond == readMnemonic.creationTime.getEpochSecond)
        case Left(err) => fail(err.toString)
      }
  }

  it must "read a mnemonic without a creation time" in { walletConf =>
    val badJson =
      """
        | {
        |   "iv":"d2aeeda5ab83d43bb0b8fe6416b12009",
        |   "cipherText": "003ad9acd6c3559911d7e2446dc329c869266844fda949d69fce591205ab7a32ddb0aa614b1be5963ecc5b784bb0c1454d5d757b71584d5d990ecadc3d4414b87df50ffc46a54c912f258d5ab094bbeb49f92ef02ab60c92a52b3f205ce91943dc6c21b15bfbc635c17b049a8eec4b0a341c48ea163d5384ebbd69c79ff175823e8fbb0849e5a223e243c81c7f7c5bca62a11b7396",
        |   "salt":"db3a6d3c88f430bf44f4a834d85255ad6b52c187c05e95fac3b427b094298028"
        | }
    """.stripMargin
    val seedPath = getSeedPath(walletConf)
    Files.write(seedPath, badJson.getBytes())

    val read =
      WalletStorage.decryptMnemonicFromDisk(seedPath,
                                            BIP39KeyManager.badPassphrase)

    read match {
      case Right(readMnemonic) =>
        assert(
          readMnemonic.creationTime.getEpochSecond == WalletStorage.FIRST_BITCOIN_S_WALLET_TIME)
      case Left(err) => fail(err.toString)
    }
  }

  it must "fail to read a mnemonic with improperly formatted creation time" in {
    walletConf =>
      val badJson =
        """
          | {
          |   "iv":"d2aeeda5ab83d43bb0b8fe6416b12009",
          |   "cipherText": "003ad9acd6c3559911d7e2446dc329c869266844fda949d69fce591205ab7a32ddb0aa614b1be5963ecc5b784bb0c1454d5d757b71584d5d990ecadc3d4414b87df50ffc46a54c912f258d5ab094bbeb49f92ef02ab60c92a52b3f205ce91943dc6c21b15bfbc635c17b049a8eec4b0a341c48ea163d5384ebbd69c79ff175823e8fbb0849e5a223e243c81c7f7c5bca62a11b7396",
          |   "salt":"db3a6d3c88f430bf44f4a834d85255ad6b52c187c05e95fac3b427b094298028",
          |   "creationTime":"not a number"
          | }
    """.stripMargin
      val seedPath = getSeedPath(walletConf)
      Files.write(seedPath, badJson.getBytes())

      assertThrows[InvalidData] {
        WalletStorage.decryptMnemonicFromDisk(seedPath,
                                              BIP39KeyManager.badPassphrase)
      }
  }

  it must "fail to read a mnemonic with bad password" in { walletConf =>
    val _ = getAndWriteMnemonic(walletConf)
    val seedPath = getSeedPath(walletConf)
    val read = WalletStorage.decryptMnemonicFromDisk(seedPath, badPassphrase)

    read match {
      case Right(mnemonic) =>
        fail("Wrote and read with different passwords")
      case Left(DecryptionError) => succeed
      case Left(err)             => fail(err.toString)
    }
  }

  it must "fail to read a mnemonic that has bad JSON in it" in { walletConf =>
    val badJson =
      """
        | {
        |   "iv":"ba7722683dad8067df8d069ee04530cc",
        |   "cipherText":,
        |   "salt":"2b7e7d718139518070a87fbbda03ea33cdcda83b555020e9344774e6e7d08af2"
        | }
    """.stripMargin
    val seedPath = getSeedPath(walletConf)
    Files.write(seedPath, badJson.getBytes())

    val read =
      WalletStorage.decryptMnemonicFromDisk(seedPath, passphrase)

    read match {
      case Left(JsonParsingError(_))  => succeed
      case res @ (Left(_) | Right(_)) => fail(res.toString())
    }
  }

  it must "fail to read a mnemonic that has missing a JSON field" in {
    walletConf =>
      val badJson =
        """
          | {
          |   "iv":"ba7722683dad8067df8d069ee04530cc",
          |   "salt":"2b7e7d718139518070a87fbbda03ea33cdcda83b555020e9344774e6e7d08af2"
          | }
    """.stripMargin
      val seedPath = getSeedPath(walletConf)
      Files.write(seedPath, badJson.getBytes())

      assertThrows[NoSuchElementException] {
        WalletStorage.decryptMnemonicFromDisk(seedPath, passphrase)
      }
  }

  it must "fail to read a mnemonic not in hex" in { walletConf =>
    val badJson =
      """
        | {
        |   "iv":"ba7722683dad8067df8d069ee04530cc",
        |   "cipherText": "my name is jeff",
        |   "salt":"2b7e7d718139518070a87fbbda03ea33cdcda83b555020e9344774e6e7d08af2"
        | }
    """.stripMargin
    val seedPath = getSeedPath(walletConf)
    Files.write(seedPath, badJson.getBytes())

    val read =
      WalletStorage.decryptMnemonicFromDisk(seedPath, passphrase)

    read match {
      case Left(JsonParsingError(_))  => succeed
      case res @ (Left(_) | Right(_)) => fail(res.toString)
    }
  }

  it must "throw an exception if we attempt to overwrite an existing seed" in {
    walletConf =>
      assert(!walletConf.seedExists())

      val _ = getAndWriteMnemonic(walletConf)

      // should have been written by now
      assert(walletConf.seedExists())

      assertThrows[RuntimeException] {
        //attempt to write another mnemonic
        getAndWriteMnemonic(walletConf)
      }
  }

  it must "write and read an ExtPrivateKey from disk" in {
    walletConf: WalletAppConfig =>
      assert(!walletConf.seedExists())

      val password = getBIP39PasswordOpt().getOrElse(BIP39Seed.EMPTY_PASSWORD)
      val keyVersion = SegWitMainNetPriv

      val writtenMnemonic = getAndWriteMnemonic(walletConf)
      val expected = BIP39Seed
        .fromMnemonic(mnemonic = writtenMnemonic.mnemonicCode,
                      password = password)
        .toExtPrivateKey(keyVersion)
        .toHardened

      // should have been written by now
      assert(walletConf.seedExists())
      val seedPath = getSeedPath(walletConf)
      val read =
        WalletStorage.getPrivateKeyFromDisk(seedPath,
                                            keyVersion,
                                            passphrase,
                                            Some(password))

      assert(read == expected)
  }
}
