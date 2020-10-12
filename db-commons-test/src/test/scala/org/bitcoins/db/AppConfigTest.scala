package org.bitcoins.db

import com.typesafe.config.ConfigFactory
import org.bitcoins.core.config._
import org.bitcoins.dlc.oracle.DLCOracleAppConfig
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.util.BitcoinSAsyncTest
import org.bitcoins.wallet.config.WalletAppConfig
import slick.jdbc.SQLiteProfile

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

  it must "create from config" in {
    val dir = BitcoinSTestAppConfig.tmpDir().toAbsolutePath
    val conf = ConfigFactory.parseString {
      s"""
         |bitcoin-s {
         |  datadir = $dir
         |}
      """.stripMargin
    }

    val appConfig = BitcoinSAppConfig.fromConfig(conf)
    assert(appConfig.walletConf.baseDatadir == dir)
    assert(appConfig.nodeConf.baseDatadir == dir)
    assert(appConfig.chainConf.baseDatadir == dir)
    assert(appConfig.bitcoindRpcConf.baseDatadir == dir)

    val dlcOracleAppConfig = DLCOracleAppConfig.fromConfig(conf)
    assert(dlcOracleAppConfig.baseDatadir == dir)
  }

  it must "create from class path config" in {
    val appConfig = BitcoinSAppConfig.fromClassPathConfig()
    assert(appConfig.nodeConf.network == RegTest)
    assert(appConfig.walletConf.requiredConfirmations == 6)
    assert(!appConfig.chainConf.forceRecalcChainWork)
  }

  it must "fill in typesafe config variables correctly" in {
    val datadir = BitcoinSTestAppConfig.tmpDir()
    val walletAppConfig = WalletAppConfig(datadir)
    for {
      _ <- walletAppConfig.start()
    } yield {
      //this should get substituted as all default sqlite
      //configuration is saved in the "bitcoin-s.sqlite" key
      //if in the future we change our default database behavior
      //this test case will need to change to check that the profile is correct
      assert(walletAppConfig.dbConfig.profile.isInstanceOf[SQLiteProfile])
    }
  }

  it should "override a configuration with a system property" in {
    val datadir = BitcoinSTestAppConfig.tmpDir()
    System.setProperty("bitcoin-s.wallet.requiredConfirmations", "1")

    //need to invalidate the config cache to force typesafe config
    //to freshly load all system properties
    ConfigFactory.invalidateCaches()

    val walletAppConfig = WalletAppConfig(datadir)
    val assertF = for {
      _ <- walletAppConfig.start()
    } yield {
      assert(walletAppConfig.requiredConfirmations == 1)
    }
    assertF.onComplete { _ =>
      System.clearProperty("bitcoin-s.wallet.requiredConfirmations")
      ConfigFactory.invalidateCaches()
    }
    assertF
  }

}
