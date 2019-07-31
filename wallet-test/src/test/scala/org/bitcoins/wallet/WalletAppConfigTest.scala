package org.bitcoins.wallet

import org.bitcoins.testkit.util.BitcoinSUnitTest
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.core.config.TestNet3
import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory
import org.bitcoins.core.config.RegTest
import org.bitcoins.core.config.MainNet
import org.bitcoins.wallet.config.WalletAppConfig
import java.nio.file.Paths
import org.bitcoins.core.hd.HDPurposes
import java.nio.file.Files
import ch.qos.logback.classic.Level
import java.nio.file.Path
import scala.util.Properties

class WalletAppConfigTest extends BitcoinSUnitTest {

  val tempDir = Files.createTempDirectory("bitcoin-s")
  val config = WalletAppConfig(directory = tempDir)

  it must "resolve DB connections correctly " in {
    assert(config.dbPath.startsWith(Properties.tmpDir))
  }

  it must "be overridable" in {
    assert(config.network == RegTest)

    val otherConf = ConfigFactory.parseString("bitcoin-s.network = testnet3")
    val withOther: WalletAppConfig = config.withOverrides(otherConf)
    assert(withOther.network == TestNet3)

    val mainnetConf = ConfigFactory.parseString("bitcoin-s.network = mainnet")
    val mainnet: WalletAppConfig = withOther.withOverrides(mainnetConf)
    assert(mainnet.network == MainNet)
  }

  it should "not matter how the overrides are passed in" in {
    val overrider = ConfigFactory.parseString(s"""
    |bitcoin-s {
    |  network = mainnet
    |}
    |""".stripMargin)

    val throughConstuctor = WalletAppConfig(tempDir, overrider)
    val throughWithOverrides = config.withOverrides(overrider)
    assert(throughWithOverrides.network == MainNet)
    assert(throughWithOverrides.network == throughConstuctor.network)

    assert(throughWithOverrides.datadir == throughConstuctor.datadir)

  }

  it must "be overridable without screwing up other options" in {
    val otherConf = ConfigFactory.parseString(
      s"bitcoin-s.wallet.defaultAccountType = segwit"
    )
    val thirdConf = ConfigFactory.parseString(
      s"bitcoin-s.wallet.defaultAccountType = nested-segwit")

    val overriden = config.withOverrides(otherConf)

    val twiceOverriden = overriden.withOverrides(thirdConf)

    assert(overriden.defaultAccountKind == HDPurposes.SegWit)
    assert(twiceOverriden.defaultAccountKind == HDPurposes.NestedSegWit)

    assert(config.datadir == overriden.datadir)
    assert(twiceOverriden.datadir == overriden.datadir)
  }

  it must "be overridable with multiple levels" in {
    val testnet = ConfigFactory.parseString("bitcoin-s.network = testnet3")
    val mainnet = ConfigFactory.parseString("bitcoin-s.network = mainnet")
    val overriden: WalletAppConfig = config.withOverrides(testnet, mainnet)
    assert(overriden.network == MainNet)
  }

  it must "have user data directory configuration take precedence" in {

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

    val appConfig = WalletAppConfig(directory = tempDir)

    assert(appConfig.datadir == tempDir.resolve("testnet3"))
    assert(appConfig.network == TestNet3)
    assert(appConfig.logLevel == Level.OFF)
    assert(appConfig.p2pLogLevel == Level.WARN)
  }
}
