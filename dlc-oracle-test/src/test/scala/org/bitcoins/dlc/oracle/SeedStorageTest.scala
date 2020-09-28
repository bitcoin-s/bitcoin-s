package org.bitcoins.dlc.oracle

import java.nio.file.{Files, Path}
import java.util.NoSuchElementException

import org.bitcoins.core.util.TimeUtil
import org.bitcoins.crypto.AesPassword
import org.bitcoins.dlc.oracle.storage.SeedStorage
import org.bitcoins.keymanager.{DecryptedMnemonic, EncryptedMnemonicHelper}
import org.bitcoins.testkit.BitcoinSTestAppConfig.tmpDir
import org.bitcoins.testkit.Implicits._
import org.bitcoins.testkit.core.gen.CryptoGenerators
import org.bitcoins.testkit.fixtures.BitcoinSFixture
import org.bitcoins.testkit.util.FileUtil
import org.scalatest.FutureOutcome
import ujson.Value.InvalidData

import scala.concurrent.Future

class SeedStorageTest extends BitcoinSFixture {

  override type FixtureParam = DLCOracleAppConfig

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val builder: () => Future[DLCOracleAppConfig] = () => {
      val conf = DLCOracleAppConfig(tmpDir())
      conf.start().map(_ => conf)
    }

    val destroy: DLCOracleAppConfig => Future[Unit] = conf => {
      FileUtil.deleteTmpDir(conf.datadir)
      conf.stop()
    }
    makeDependentFixture(builder, destroy = destroy)(test)
  }

  def getSeedPath(config: DLCOracleAppConfig): Path = {
    config.seedPath
  }

  behavior of "SeedStorage"

  val passphrase: AesPassword = AesPassword.fromNonEmptyString("this_is_secret")

  val badPassphrase: AesPassword =
    AesPassword.fromNonEmptyString("this_is_also_secret")

  def getAndWriteMnemonic(conf: DLCOracleAppConfig): DecryptedMnemonic = {
    val mnemonicCode = CryptoGenerators.mnemonicCode.sampleSome
    val decryptedMnemonic = DecryptedMnemonic(mnemonicCode, TimeUtil.now)
    val encrypted =
      EncryptedMnemonicHelper.encrypt(decryptedMnemonic, passphrase)
    val seedPath = getSeedPath(conf)
    val _ =
      SeedStorage.writeMnemonicToDisk(seedPath, encrypted)
    decryptedMnemonic
  }

  it must "write and read a mnemonic to disk" in { conf: DLCOracleAppConfig =>
    assert(!conf.seedExists())

    val writtenMnemonic = getAndWriteMnemonic(conf)

    // should have been written by now
    assert(conf.seedExists())
    val seedPath = getSeedPath(conf)
    val readMnemonic = SeedStorage.decryptMnemonicFromDisk(seedPath, passphrase)

    assert(writtenMnemonic.mnemonicCode == readMnemonic.mnemonicCode)
    // Need to compare using getEpochSecond because when reading an epoch second
    // it will not include the milliseconds that writtenMnemonic will have
    assert(
      writtenMnemonic.creationTime.getEpochSecond == readMnemonic.creationTime.getEpochSecond)
  }

  it must "read a mnemonic without a creation time" in { conf =>
    val badJson =
      """
        | {
        |   "iv":"d2aeeda5ab83d43bb0b8fe6416b12009",
        |   "cipherText": "003ad9acd6c3559911d7e2446dc329c869266844fda949d69fce591205ab7a32ddb0aa614b1be5963ecc5b784bb0c1454d5d757b71584d5d990ecadc3d4414b87df50ffc46a54c912f258d5ab094bbeb49f92ef02ab60c92a52b3f205ce91943dc6c21b15bfbc635c17b049a8eec4b0a341c48ea163d5384ebbd69c79ff175823e8fbb0849e5a223e243c81c7f7c5bca62a11b7396",
        |   "salt":"db3a6d3c88f430bf44f4a834d85255ad6b52c187c05e95fac3b427b094298028"
        | }
    """.stripMargin
    val seedPath = getSeedPath(conf)
    Files.write(seedPath, badJson.getBytes())

    val readMnemonic =
      SeedStorage.decryptMnemonicFromDisk(seedPath,
                                          AesPassword.fromString("changeMe"))

    assert(
      readMnemonic.creationTime.getEpochSecond == SeedStorage.FIRST_WALLET_TIME)
  }

  it must "fail to read a mnemonic with improperly formatted creation time" in {
    conf =>
      val badJson =
        """
          | {
          |   "iv":"d2aeeda5ab83d43bb0b8fe6416b12009",
          |   "cipherText": "003ad9acd6c3559911d7e2446dc329c869266844fda949d69fce591205ab7a32ddb0aa614b1be5963ecc5b784bb0c1454d5d757b71584d5d990ecadc3d4414b87df50ffc46a54c912f258d5ab094bbeb49f92ef02ab60c92a52b3f205ce91943dc6c21b15bfbc635c17b049a8eec4b0a341c48ea163d5384ebbd69c79ff175823e8fbb0849e5a223e243c81c7f7c5bca62a11b7396",
          |   "salt":"db3a6d3c88f430bf44f4a834d85255ad6b52c187c05e95fac3b427b094298028",
          |   "creationTime":"not a number"
          | }
    """.stripMargin
      val seedPath = getSeedPath(conf)
      Files.write(seedPath, badJson.getBytes())

      assertThrows[InvalidData] {
        SeedStorage.decryptMnemonicFromDisk(seedPath, badPassphrase)
      }
  }

  it must "fail to read a mnemonic with bad password" in { conf =>
    val _ = getAndWriteMnemonic(conf)
    val seedPath = getSeedPath(conf)

    assertThrows[RuntimeException] {
      SeedStorage.decryptMnemonicFromDisk(seedPath, badPassphrase)
    }
  }

  it must "fail to read a mnemonic that has bad JSON in it" in { conf =>
    val badJson =
      """
        | {
        |   "iv":"ba7722683dad8067df8d069ee04530cc",
        |   "cipherText":,
        |   "salt":"2b7e7d718139518070a87fbbda03ea33cdcda83b555020e9344774e6e7d08af2"
        | }
    """.stripMargin
    val seedPath = getSeedPath(conf)
    Files.write(seedPath, badJson.getBytes())

    assertThrows[RuntimeException] {
      SeedStorage.decryptMnemonicFromDisk(seedPath, passphrase)
    }
  }

  it must "fail to read a mnemonic that has missing a JSON field" in { conf =>
    val badJson =
      """
        | {
        |   "iv":"ba7722683dad8067df8d069ee04530cc",
        |   "salt":"2b7e7d718139518070a87fbbda03ea33cdcda83b555020e9344774e6e7d08af2"
        | }
    """.stripMargin
    val seedPath = getSeedPath(conf)
    Files.write(seedPath, badJson.getBytes())

    assertThrows[NoSuchElementException] {
      SeedStorage.decryptMnemonicFromDisk(seedPath, passphrase)
    }
  }

  it must "fail to read a mnemonic not in hex" in { conf =>
    val badJson =
      """
        | {
        |   "iv":"ba7722683dad8067df8d069ee04530cc",
        |   "cipherText": "my name is jeff",
        |   "salt":"2b7e7d718139518070a87fbbda03ea33cdcda83b555020e9344774e6e7d08af2"
        | }
    """.stripMargin
    val seedPath = getSeedPath(conf)
    Files.write(seedPath, badJson.getBytes())

    assertThrows[RuntimeException] {
      SeedStorage.decryptMnemonicFromDisk(seedPath, passphrase)
    }
  }

  it must "throw an exception if we attempt to overwrite an existing seed" in {
    conf =>
      assert(!conf.seedExists())

      val _ = getAndWriteMnemonic(conf)

      // should have been written by now
      assert(conf.seedExists())

      assertThrows[RuntimeException] {
        //attempt to write another mnemonic
        getAndWriteMnemonic(conf)
      }
  }

}
