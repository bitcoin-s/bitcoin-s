package org.bitcoins.testkit.fixtures

import com.typesafe.config.{Config, ConfigFactory}
import org.bitcoins.rpc.config.BitcoindInstance
import org.bitcoins.rpc.util.RpcUtil
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.rpc.CachedBitcoindNewest
import org.bitcoins.testkit.util.TorUtil
import org.bitcoins.testkit.{BitcoinSTestAppConfig, EmbeddedPg}
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
        datadir = bitcoind.instance.datadir
        conf = buildConfig(bitcoind.instance)
        bitcoinSAppConfig = BitcoinSAppConfig(datadir.toPath, conf)
      } yield bitcoinSAppConfig
    }

    val destroyF: BitcoinSAppConfig => Future[Unit] = { appConfig =>
      val stopF = appConfig
        .stop()
        .map(_ => BitcoinSTestAppConfig.deleteAppConfig(appConfig))
        .map(_ => ())
      stopF
    }

    makeDependentFixture(builder, destroyF)(test)
  }

  /** Builds a configuration with the proper bitcoind credentials and bitcoin-s node mode set to bitcoind
    * and sets tor config
    */
  private def buildConfig(instance: BitcoindInstance): Config = {
    val configStr =
      s"""
         |bitcoin-s.bitcoind-rpc.rpcuser="${instance.authCredentials.username}"
         |bitcoin-s.bitcoind-rpc.rpcpassword="${instance.authCredentials.password}"
         |bitcoin-s.bitcoind-rpc.rpcbind="${instance.rpcUri.getHost}"
         |bitcoin-s.bitcoind-rpc.rpcport="${instance.rpcUri.getPort}"
         |bitcoin-s.bitcoind-rpc.isRemote=true
         |bitcoin-s.bitcoind-rpc.version="${instance.getVersion}"
         |bitcoin-s.node.mode=bitcoind
         |bitcoin-s.tor.enabled=${TorUtil.torEnabled}
         |bitcoin-s.proxy.enabled=${TorUtil.torEnabled}
         |bitcoin-s.dlcnode.listen = "0.0.0.0:${RpcUtil.randomPort}"
         |bitcoin-s.server.rpcport = ${RpcUtil.randomPort}
         |""".stripMargin

    ConfigFactory.parseString(configStr)
  }

  override def afterAll(): Unit = {
    super[CachedBitcoindNewest].afterAll()
    super[BitcoinSAppConfigFixture].afterAll()
  }
}
