package org.bitcoins.testkit.node

import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.commons.config.AppConfig
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.tor.CachedTor
import org.bitcoins.testkit.util.BitcoinSAkkaAsyncTest
import org.bitcoins.wallet.config.WalletAppConfig

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt

sealed trait CachedAppConfig { _: BitcoinSAkkaAsyncTest =>

  /** Unfortunately this can't be a 'val' because of NPE */
  implicit protected def appConfig: AppConfig

  override def afterAll(): Unit = {
    Await.result(appConfig.stop(), duration)
  }
}

trait CachedBitcoinSAppConfig { _: BitcoinSAkkaAsyncTest =>

  implicit protected lazy val cachedConfig: BitcoinSAppConfig =
    BitcoinSTestAppConfig.getNeutrinoTestConfig(Vector.empty)

  implicit protected lazy val cachedNodeConf: NodeAppConfig = {
    cachedConfig.nodeConf
  }

  implicit protected lazy val cachedWalletConf: WalletAppConfig = {
    cachedConfig.walletConf
  }

  implicit protected lazy val cachedChainConf: ChainAppConfig = {
    cachedConfig.chainConf
  }

  override def beforeAll(): Unit = {
    //takes awhile for tor to start
    Await.result(cachedConfig.start(), 45.seconds)
  }

  override def afterAll(): Unit = {
    Await.result(cachedConfig.stop(), duration)
  }
}

trait CachedBitcoinSAppConfigCachedTor
    extends CachedBitcoinSAppConfig
    with CachedTor {
  _: BitcoinSAkkaAsyncTest =>

  override def beforeAll(): Unit = {
    super[CachedTor].beforeAll()
    super[CachedBitcoinSAppConfig].beforeAll()
  }

  override def afterAll(): Unit = {
    super[CachedBitcoinSAppConfig].afterAll()
    super[CachedTor].afterAll()
  }

  implicit override protected lazy val cachedConfig: BitcoinSAppConfig =
    BitcoinSTestAppConfig.getNeutrinoTestConfig(Vector.empty, Some(torConfig))
}

trait CachedChainAppConfig {
  _: BitcoinSAkkaAsyncTest =>

  private[this] lazy val cachedConfig: BitcoinSAppConfig =
    BitcoinSTestAppConfig.getSpvTestConfig()

  implicit protected lazy val cachedChainConf: ChainAppConfig = {
    cachedConfig.chainConf
  }

  override def afterAll(): Unit = {
    Await.result(cachedChainConf.stop(), duration)
  }
}
