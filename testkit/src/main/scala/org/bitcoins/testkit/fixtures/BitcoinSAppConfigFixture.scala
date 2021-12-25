package org.bitcoins.testkit.fixtures

import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.EmbeddedPg
import org.bitcoins.testkit.rpc.CachedBitcoindNewest
import org.bitcoins.testkit.server.BitcoinSServerMainUtil
import org.scalatest.FutureOutcome

import scala.concurrent.Future

sealed trait BitcoinSAppConfigFixture extends BitcoinSFixture with EmbeddedPg {
  override type FixtureParam = BitcoinSAppConfig

  override def afterAll(): Unit = {
    super[EmbeddedPg].afterAll()
    super[BitcoinSFixture].afterAll()
  }
}

/** Makes a bitcoin-s app config with proper bitcoind credentials
  * and bitcoin-s.node.mode=bitcoind to use bitcoin as the backend
  *
  * The [[BitcoinSAppConfig]] is not started
  */
trait BitcoinSAppConfigBitcoinFixtureNotStarted
    extends BitcoinSAppConfigFixture
    with CachedBitcoindNewest {

  override type FixtureParam = BitcoinSAppConfig

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val builder: () => Future[BitcoinSAppConfig] = () => {
      for {
        bitcoind <- cachedBitcoindWithFundsF
        bitcoinSAppConfig = BitcoinSServerMainUtil
          .buildBitcoindBitcoinSAppConfig(bitcoind)
      } yield bitcoinSAppConfig
    }

    val destroyF: BitcoinSAppConfig => Future[Unit] = { appConfig =>
      BitcoinSServerMainUtil.destroyBitcoinSAppConfig(appConfig)
    }

    makeDependentFixture(builder, destroyF)(test)
  }

  override def afterAll(): Unit = {
    super[CachedBitcoindNewest].afterAll()
    super[BitcoinSAppConfigFixture].afterAll()
  }
}
