package org.bitcoins.wallet

import java.nio.file.Files
import com.typesafe.config.ConfigFactory
import org.bitcoins.core.config.{MainNet, RegTest, TestNet3}
import org.bitcoins.core.hd.HDPurpose
import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.bitcoins.wallet.config.WalletAppConfig
import org.scalatest.FutureOutcome

class WalletAppConfigTest extends BitcoinSWalletTest {

  override type FixtureParam = WalletAppConfig
  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    withWalletConfig(test)
  }

  it must "be overridable" in { (config: WalletAppConfig) =>
    assert(config.network == RegTest)

    val otherConf = ConfigFactory.parseString("bitcoin-s.network = testnet3")
    val withOther: WalletAppConfig = config.withOverrides(otherConf)
    assert(withOther.network == TestNet3)

    val mainnetConf = ConfigFactory.parseString("bitcoin-s.network = mainnet")
    val mainnet: WalletAppConfig = withOther.withOverrides(mainnetConf)
    assert(mainnet.network == MainNet)
  }

  it should "not matter how the overrides are passed in" in {
    (config: WalletAppConfig) =>
      val overrider = ConfigFactory.parseString(s"""
                                                 |bitcoin-s {
                                                 |  network = mainnet
                                                 |}
                                                 |""".stripMargin)

      val throughConstructor =
        WalletAppConfig(config.datadir, Vector(overrider))
      val throughWithOverrides = config.withOverrides(overrider)
      assert(throughWithOverrides.network == MainNet)
      assert(throughWithOverrides.network == throughConstructor.network)

      // assert(throughWithOverrides.datadir == throughConstructor.datadir)

  }

  it must "be overridable without screwing up other options" in {
    (config: WalletAppConfig) =>
      val otherConf = ConfigFactory.parseString(
        s"bitcoin-s.wallet.purpose = segwit"
      )
      val thirdConf = ConfigFactory.parseString(
        s"bitcoin-s.wallet.purpose = nested-segwit"
      )

      val overriden = config.withOverrides(otherConf)

      val twiceOverriden = overriden.withOverrides(thirdConf)

      assert(overriden.defaultPurpose == HDPurpose.SegWit)
      assert(twiceOverriden.defaultPurpose == HDPurpose.NestedSegWit)

      assert(config.datadir == overriden.datadir)
      assert(twiceOverriden.datadir == overriden.datadir)
  }

  it must "be overridable with multiple levels" in {
    (config: WalletAppConfig) =>
      val testnet = ConfigFactory.parseString("bitcoin-s.network = testnet3")
      val mainnet = ConfigFactory.parseString("bitcoin-s.network = mainnet")
      val overriden: WalletAppConfig =
        config.withOverrides(Vector(testnet, mainnet))
      assert(overriden.network == MainNet)
  }

  it must "have user data directory configuration take precedence" in {
    (_: WalletAppConfig) =>
      val tempDir = Files.createTempDirectory("bitcoin-s")
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

      val appConfig = WalletAppConfig(baseDatadir = tempDir, Vector.empty)

      assert(appConfig.datadir == tempDir.resolve("testnet3"))
      assert(appConfig.network == TestNet3)
  }

  it must "fail to start the wallet app config if we have different seeds" in {
    (config: WalletAppConfig) =>
      val seedFile = config.seedPath
      val startedF = config.start()

      // stop old oracle
      val stoppedF = for {
        _ <- startedF
        _ <- config.stop()
      } yield ()

      val deletedF = for {
        _ <- stoppedF
      } yield {
        // delete the seed so we start with a new seed
        Files.delete(seedFile)
      }

      val start2F = for {
        _ <- deletedF
        _ <- config.start()
      } yield ()

      // start it again and except an exception
      recoverToSucceededIf[RuntimeException] {
        start2F
      }
  }

  it must "be able to reuse database connections across after starting/stopping the app config" in {
    (config: WalletAppConfig) =>
      val stoppedF = for {
        hasWallet <- config.hasWallet()(config, executionContext)
        _ = assert(!hasWallet)
        _ <- config.stop()
        // fails because db connection pool not started
        _ = assertThrows[RuntimeException](
          config.hasWallet()(config, executionContext))
        _ <- config.start() // restart so fixture can tear it down correctly
      } yield {}

      stoppedF.map(_ => succeed)
  }
}
