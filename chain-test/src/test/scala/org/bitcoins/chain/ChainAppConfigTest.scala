package org.bitcoins.chain

import com.typesafe.config.ConfigFactory
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.core.config.{MainNet, RegTest, TestNet3}
import org.bitcoins.testkit.util.BitcoinSUnitTest

class ChainAppConfigTest extends BitcoinSUnitTest {
  val config = ChainAppConfig()

  it must "be overridable" in {
    assert(config.network == RegTest)

    val otherConf = ConfigFactory.parseString("bitcoin-s.network = testnet3")
    val withOther: ChainAppConfig = config.withOverrides(otherConf)
    assert(withOther.network == TestNet3)

    val mainnetConf = ConfigFactory.parseString("bitcoin-s.network = mainnet")
    val mainnet: ChainAppConfig = withOther.withOverrides(mainnetConf)
    assert(mainnet.network == MainNet)
  }

  it must "be overridable with multiple levels" in {
    val testnet = ConfigFactory.parseString("bitcoin-s.network = testnet3")
    val mainnet = ConfigFactory.parseString("bitcoin-s.network = mainnet")
    val overriden: ChainAppConfig = config.withOverrides(testnet, mainnet)
    assert(overriden.network == MainNet)

  }
}
