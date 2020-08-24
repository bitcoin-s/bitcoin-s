package org.bitcoins.testkit.node

import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.db.AppConfig
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.util.{BaseAsyncTest}

import scala.concurrent.Await

sealed trait CachedAppConfig { _: BaseAsyncTest =>

  /** Unfortunately this can't be a 'val' because of NPE */
  implicit protected def appConfig: AppConfig

  override def afterAll(): Unit = {
    Await.result(appConfig.stop(), akkaTimeout.duration)
  }
}

trait CachedChainAppConfig extends CachedAppConfig { _: BaseAsyncTest =>

  implicit protected def appConfig: ChainAppConfig
}

trait CachedBitcoinSAppConfig { _: BaseAsyncTest =>

  implicit protected lazy val config: BitcoinSAppConfig =
    BitcoinSTestAppConfig.getSpvTestConfig()

  implicit protected lazy val nodeConf: NodeAppConfig = {
    config.nodeConf
  }

  override def afterAll(): Unit = {
    Await.result(config.stop(), akkaTimeout.duration)
  }
}
