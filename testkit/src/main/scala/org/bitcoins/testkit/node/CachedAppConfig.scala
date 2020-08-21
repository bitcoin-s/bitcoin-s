package org.bitcoins.testkit.node

import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.util.BitcoinSAsyncTest

import scala.concurrent.Await

trait CachedAppConfig { _: BitcoinSAsyncTest =>

  implicit protected lazy val config: BitcoinSAppConfig =
    BitcoinSTestAppConfig.getSpvTestConfig()

  override def afterAll(): Unit = {
    Await.result(config.stop(), akkaTimeout.duration)
  }
}
