package org.bitcoins.testkit.fixtures

import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.{BitcoinSTestAppConfig, EmbeddedPg}
import org.bitcoins.testkit.rpc.CachedBitcoindNewest
import org.bitcoins.testkit.server.BitcoinSServerMainUtil
import org.scalatest.FutureOutcome

import scala.concurrent.Future

sealed trait BitcoinSAppConfigFixture extends BitcoinSFixture with EmbeddedPg {

  override def afterAll(): Unit = {
    super[EmbeddedPg].afterAll()
    super[BitcoinSFixture].afterAll()
  }
}

/** Makes a bitcoin-s app config with proper bitcoind credentials and
  * bitcoin-s.node.mode=bitcoind to use bitcoin as the backend
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

trait BitcoinSAppConfigBitcoinFixtureStarted
    extends BitcoinSAppConfigFixture
    with CachedBitcoindNewest {

  def withTwoBitcoinSAppConfigNotStarted(
      test: OneArgAsyncTest
  ): FutureOutcome = {
    val builder: () => Future[(BitcoinSAppConfig, BitcoinSAppConfig)] = () => {
      for {
        _ <- cachedBitcoindWithFundsF
        bitcoinSAppConfig1 = BitcoinSTestAppConfig
          .getNeutrinoWithEmbeddedDbTestConfig(
            postgresOpt = postgresOpt,
            config = Vector.empty,
            forceNamedWallet = true
          )
        bitcoinSAppConfig2 = BitcoinSTestAppConfig
          .getNeutrinoWithEmbeddedDbTestConfig(
            postgresOpt = postgresOpt,
            config = Vector.empty,
            forceNamedWallet = true
          )
        _ <- bitcoinSAppConfig1.start()
        _ <- bitcoinSAppConfig2.start()
      } yield (bitcoinSAppConfig1, bitcoinSAppConfig2)
    }

    val destroyF: ((BitcoinSAppConfig, BitcoinSAppConfig)) => Future[Unit] = {
      case (appConfig1, appConfig2) =>
        val destroy1F =
          BitcoinSServerMainUtil.destroyBitcoinSAppConfig(appConfig1)
        val destroy2F =
          BitcoinSServerMainUtil.destroyBitcoinSAppConfig(appConfig2)
        for {
          _ <- destroy1F
          _ <- destroy2F
        } yield ()
    }

    makeDependentFixture[(BitcoinSAppConfig, BitcoinSAppConfig)](
      builder,
      destroyF
    )(test)
  }

  override def afterAll(): Unit = {
    super[CachedBitcoindNewest].afterAll()
    super[BitcoinSAppConfigFixture].afterAll()
  }
}
