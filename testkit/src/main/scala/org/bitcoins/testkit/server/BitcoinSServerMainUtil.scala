package org.bitcoins.testkit.server

import akka.actor.ActorSystem
import com.typesafe.config.{Config, ConfigFactory}
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.config.{
  BitcoindInstance,
  BitcoindInstanceLocal,
  BitcoindInstanceRemote
}
import org.bitcoins.rpc.util.RpcUtil
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.util.{FileUtil, TorUtil}

import scala.concurrent.{ExecutionContext, Future}

object BitcoinSServerMainUtil {

  /** Builds a configuration with the proper bitcoind credentials and bitcoin-s node mode set to bitcoind
    * and sets tor config
    */
  def buildBitcoindConfig(instance: BitcoindInstance): Config = {
    val version = instance match {
      case local: BitcoindInstanceLocal => local.getVersion
      case _: BitcoindInstanceRemote =>
        sys.error("Remote instance should not be used in tests")
    }
    val configStr =
      s"""
         |bitcoin-s.bitcoind-rpc.rpcuser="${instance.authCredentials.username}"
         |bitcoin-s.bitcoind-rpc.rpcpassword="${instance.authCredentials.password}"
         |bitcoin-s.bitcoind-rpc.rpcbind="${instance.rpcUri.getHost}"
         |bitcoin-s.bitcoind-rpc.rpcport="${instance.rpcUri.getPort}"
         |bitcoin-s.bitcoind-rpc.remote=true
         |bitcoin-s.bitcoind-rpc.version="${version}"
         |bitcoin-s.node.mode=bitcoind
         |bitcoin-s.tor.enabled=${TorUtil.torEnabled}
         |bitcoin-s.proxy.enabled=${TorUtil.torEnabled}
         |bitcoin-s.dlcnode.listen = "0.0.0.0:${RpcUtil.randomPort}"
         |bitcoin-s.server.rpcport = ${RpcUtil.randomPort}
         |bitcoin-s.server.wsport= ${RpcUtil.randomPort}
         |bitcoin-s.server.password=topsecret
         |""".stripMargin

    ConfigFactory.parseString(configStr)
  }

  /** Builds a [[BitcoinSAppConfig]] that uses a bitcoind backend */
  def buildBitcoindBitcoinSAppConfig(bitcoind: BitcoindRpcClient)(implicit
      system: ActorSystem): BitcoinSAppConfig = {
    val conf = BitcoinSServerMainUtil.buildBitcoindConfig(bitcoind.instance)
    val datadir = FileUtil.tmpDir()
    BitcoinSAppConfig(datadir.toPath, Vector(conf))
  }

  def destroyBitcoinSAppConfig(appConfig: BitcoinSAppConfig)(implicit
      ec: ExecutionContext): Future[Unit] = {
    val stopF = appConfig
      .stop()
      .map(_ => BitcoinSTestAppConfig.deleteAppConfig(appConfig))
      .map(_ => ())
    stopF
  }
}
