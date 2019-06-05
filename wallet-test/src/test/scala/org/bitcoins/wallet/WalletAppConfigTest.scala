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

class WalletAppConfigTest extends BitcoinSUnitTest {
  val config = WalletAppConfig()

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
    val dir = Paths.get("/", "bar", "biz")
    val overrider = ConfigFactory.parseString(s"""
    |bitcoin-s {
    |  datadir = $dir 
    |  network = mainnet
    |}
    |""".stripMargin)

    val throughConstuctor = WalletAppConfig(overrider)
    val throughWithOverrides = config.withOverrides(overrider)
    assert(throughWithOverrides.network == MainNet)
    assert(throughWithOverrides.network == throughConstuctor.network)

    assert(throughWithOverrides.datadir.startsWith(dir))
    assert(throughWithOverrides.datadir == throughConstuctor.datadir)

  }

  it must "be overridable without screwing up other options" in {
    val dir = Paths.get("/", "foo", "bar")
    val otherConf = ConfigFactory.parseString(s"bitcoin-s.datadir = $dir")
    val thirdConf = ConfigFactory.parseString(
      s"bitcoin-s.wallet.defaultAccountType = nested-segwit")

    val overriden = config.withOverrides(otherConf)

    val twiceOverriden = overriden.withOverrides(thirdConf)

    assert(overriden.datadir.startsWith(dir))
    assert(twiceOverriden.datadir.startsWith(dir))
    assert(twiceOverriden.defaultAccountKind == HDPurposes.NestedSegWit)
  }

  it must "be overridable with multiple levels" in {
    val testnet = ConfigFactory.parseString("bitcoin-s.network = testnet3")
    val mainnet = ConfigFactory.parseString("bitcoin-s.network = mainnet")
    val overriden: WalletAppConfig = config.withOverrides(testnet, mainnet)
    assert(overriden.network == MainNet)

  }
}
