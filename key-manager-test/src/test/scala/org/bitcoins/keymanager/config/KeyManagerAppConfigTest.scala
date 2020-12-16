package org.bitcoins.keymanager.config

import com.typesafe.config.ConfigFactory
import org.bitcoins.core.config.{MainNet, RegTest, TestNet3}
import org.bitcoins.core.util.TimeUtil
import org.bitcoins.keymanager.{DecryptedMnemonic, WalletStorage}
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.Implicits.GeneratorOps
import org.bitcoins.testkit.core.gen.CryptoGenerators
import org.bitcoins.testkit.util.BitcoinSAsyncTest

import java.nio.file.{Files, Path}

class KeyManagerAppConfigTest extends BitcoinSAsyncTest {

  val tempDir: Path = BitcoinSTestAppConfig.tmpDir()

  val config: KeyManagerAppConfig = KeyManagerAppConfig.fromDatadir(tempDir)

  it must "be overridable" in {
    assert(config.network == RegTest)

    val otherConf = ConfigFactory.parseString("bitcoin-s.network = testnet3")
    val withOther: KeyManagerAppConfig = config.withOverrides(otherConf)
    assert(withOther.network == TestNet3)

    val mainnetConf = ConfigFactory.parseString("bitcoin-s.network = mainnet")
    val mainnet: KeyManagerAppConfig = withOther.withOverrides(mainnetConf)
    assert(mainnet.network == MainNet)
  }

  it should "not matter how the overrides are passed in" in {
    val overrider = ConfigFactory.parseString(s"""
                                                 |bitcoin-s {
                                                 |  network = mainnet
                                                 |}
                                                 |""".stripMargin)

    val throughConstructor = KeyManagerAppConfig(tempDir, overrider)
    val throughWithOverrides = config.withOverrides(overrider)
    assert(throughWithOverrides.network == MainNet)
    assert(throughWithOverrides.network == throughConstructor.network)

    assert(throughWithOverrides.datadir == throughConstructor.datadir)

  }

  it must "be overridable without screwing up other options" in {
    val otherConf = ConfigFactory.parseString(
      s"bitcoin-s.keymanager.bip39password = passA"
    )
    val thirdConf =
      ConfigFactory.parseString(s"bitcoin-s.keymanager.bip39password = passB")

    val overridden = config.withOverrides(otherConf)

    val twiceOverridden = overridden.withOverrides(thirdConf)

    assert(overridden.bip39PasswordOpt.contains("passA"))
    assert(twiceOverridden.bip39PasswordOpt.contains("passB"))

    assert(config.datadir == overridden.datadir)
    assert(twiceOverridden.datadir == overridden.datadir)
  }

  it must "be overridable with multiple levels" in {
    val testnet = ConfigFactory.parseString("bitcoin-s.network = testnet3")
    val mainnet = ConfigFactory.parseString("bitcoin-s.network = mainnet")
    val overridden: KeyManagerAppConfig = config.withOverrides(testnet, mainnet)
    assert(overridden.network == MainNet)
  }

  it must "have user data directory configuration take precedence" in {
    val tempFile = Files.createFile(tempDir.resolve("bitcoin-s.conf"))
    val confStr = """
                    | bitcoin-s {
                    |   network = testnet3
                    |
                    |   logging {
                    |     level = off
                    |
                    |     p2p = warn
                    |   }
                    | }
    """.stripMargin
    val _ = Files.write(tempFile, confStr.getBytes())

    val appConfig = KeyManagerAppConfig(directory = tempDir)

    assert(appConfig.datadir == tempDir.resolve("testnet3"))
    assert(appConfig.network == TestNet3)
  }

  it must "move an old seed into the seeds folder" in {
    // generate and write mnemonic
    val mnemonicCode = CryptoGenerators.mnemonicCode.sampleSome
    val mnemonic = DecryptedMnemonic(mnemonicCode, TimeUtil.now)
    val seedPath = tempDir.resolve(WalletStorage.ENCRYPTED_SEED_FILE_NAME)
    WalletStorage.writeSeedToDisk(seedPath, mnemonic)

    config.start().map { _ =>
      assert(Files.exists(seedPath), "We should not delete the old seed!")
      assert(
        Files.exists(
          tempDir
            .resolve(WalletStorage.SEED_FOLDER_NAME)
            .resolve(WalletStorage.ENCRYPTED_SEED_FILE_NAME)))
    }
  }
}
