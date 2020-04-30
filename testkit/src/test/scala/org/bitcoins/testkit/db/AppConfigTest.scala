package org.bitcoins.testkit.db

import com.typesafe.config.ConfigFactory
import org.bitcoins.core.config.TestNet3
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.util.BitcoinSAsyncTest

class AppConfigTest extends BitcoinSAsyncTest {

  behavior of "BitcoinSAppConfig"

  it must "propagate values correctly to all sub configs" in {
    val networkOverride =
      ConfigFactory.parseString("bitcoin-s.network = testnet3")

    val config = BitcoinSTestAppConfig.getSpvTestConfig(networkOverride)
    val chainConf = config.chainConf
    val walletConf = config.walletConf
    val nodeConf = config.nodeConf

    assert(chainConf.datadir == walletConf.datadir)
    assert(walletConf.datadir == nodeConf.datadir)

    assert(chainConf.network == TestNet3)
    assert(walletConf.network == TestNet3)
    assert(nodeConf.network == TestNet3)
  }

  it must "have the same DB path" in {
    val conf = BitcoinSTestAppConfig.getSpvTestConfig()
    val chainConf = conf.chainConf
    val walletConf = conf.walletConf
    val nodeConf = conf.nodeConf
    assert(chainConf.dbPath == walletConf.dbPath)
    assert(walletConf.dbPath == nodeConf.dbPath)
  }

  it must "have distinct databases" in {
    val conf = BitcoinSTestAppConfig.getSpvTestConfig()
    val chainConf = conf.chainConf
    val walletConf = conf.walletConf
    val nodeConf = conf.nodeConf
    assert(chainConf.dbName != walletConf.dbName)
    assert(walletConf.dbName != nodeConf.dbName)
  }

}
