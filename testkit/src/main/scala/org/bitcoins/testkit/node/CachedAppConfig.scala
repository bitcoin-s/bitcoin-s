package org.bitcoins.testkit.node

import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.db.AppConfig
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.util.BitcoinSAkkaAsyncTest
import org.bitcoins.wallet.config.WalletAppConfig

import scala.concurrent.Await

sealed trait CachedAppConfig { _: BitcoinSAkkaAsyncTest =>

  /** Unfortunately this can't be a 'val' because of NPE */
  implicit protected def appConfig: AppConfig

  override def afterAll(): Unit = {
    Await.result(appConfig.stop(), duration)
  }
}

trait CachedBitcoinSAppConfig { _: BitcoinSAkkaAsyncTest =>

  implicit protected lazy val cachedConfig: BitcoinSAppConfig =
    BitcoinSTestAppConfig.getSpvTestConfig()

  implicit protected lazy val cachedNodeConf: NodeAppConfig = {
    cachedConfig.nodeConf
  }

  implicit protected lazy val cachedWalletConf: WalletAppConfig = {
    cachedConfig.walletConf
  }

  implicit protected lazy val cachedChainConf: ChainAppConfig = {
    cachedConfig.chainConf
  }

  override def afterAll(): Unit = {
    Await.result(cachedConfig.stop(), duration)
  }
}

trait CachedChainAppConfig extends CachedBitcoinSAppConfig {
  _: BitcoinSAkkaAsyncTest =>

}
