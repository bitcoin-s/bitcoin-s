package org.bitcoins.server

import java.nio.file.{Path, Paths}

import org.bitcoins.core.util.{BitcoinSLogger, StartStop}
import org.bitcoins.eclair.rpc.client.EclairRpcClient
import org.bitcoins.eclair.rpc.config.EclairInstance
import org.bitcoins.node.config.EclairAppConfig
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.client.common.BitcoindVersion._
import org.bitcoins.rpc.config.BitcoindInstance

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Properties

case class EclairBitcoindPair(
    eclair: EclairRpcClient,
    bitcoind: BitcoindRpcClient)(implicit ec: ExecutionContext)
    extends StartStop[EclairBitcoindPair]
    with BitcoinSLogger {

  require(
    eclair.instance.network == bitcoind.instance.network,
    s"Eclair and bitcoind must be on the same network, Eclair: ${eclair.instance.network} bitcoind: ${bitcoind.instance.network}"
  )

  require(eclair.instance.zmqConfig.isDefined, "Eclair must have a zmq config")
  require(
    eclair.instance.bitcoindAuthCredentials.get == bitcoind.instance.authCredentials,
    "Eclair's bitcoind authCredentials must match bitcoind's")
  require(
    eclair.instance.bitcoindRpcUri.get == bitcoind.instance.rpcUri,
    s"Eclair's bitcoind rpc uri must match bitcoind's, ${eclair.instance.bitcoindRpcUri.get} != ${bitcoind.instance.rpcUri}"
  )

  require(
    eclair.instance.zmqConfig.get.rawBlock == bitcoind.instance.zmqConfig.rawBlock,
    s"Eclair's bitcoind zmq raw block must match bitcoind's, ${eclair.instance.zmqConfig.get.rawBlock} != ${bitcoind.instance.zmqConfig.rawBlock}"
  )
  require(
    eclair.instance.zmqConfig.get.rawTx == bitcoind.instance.zmqConfig.rawTx,
    s"Eclair's bitcoind zmq raw tx must match bitcoind's, ${eclair.instance.zmqConfig.get.rawTx} != ${bitcoind.instance.zmqConfig.rawTx}"
  )

  override def start(): Future[EclairBitcoindPair] = {
    logger.info("Starting bitcoind")
    val startBitcoindF = bitcoind.start()
    for {
      startedBitcoind <- startBitcoindF

      _ = logger.info("Starting eclair")
      _ <- eclair.start()
    } yield EclairBitcoindPair(eclair, startedBitcoind)
  }

  override def stop(): Future[EclairBitcoindPair] = {
    for {
      stoppedEclair <- eclair.stop()
      stoppedBitcoind <- bitcoind.stop()
    } yield EclairBitcoindPair(stoppedEclair, stoppedBitcoind)
  }
}

object EclairBitcoindPair {

  def fromConfig(eclairConf: EclairAppConfig)(implicit
      ec: ExecutionContext): EclairBitcoindPair = {
    require(eclairConf.enabled, "Eclair is not enabled")
    val bitcoindLocation =
      getBitcoindBinary(binaryDirectory, eclairConf.bitcoindVersion)
    val bitcoindInstance = BitcoindInstance.fromDatadir(
      eclairConf.bitcoindDataDir.toFile,
      bitcoindLocation)
    val bitcoind = BitcoindRpcClient(bitcoindInstance)

    val eclairInstance =
      EclairInstance.fromDatadir(eclairConf.eclairDataDir.toFile, None)

    val eclair = EclairRpcClient(
      eclairInstance,
      EclairRpcClient.getEclairBinary(binaryDirectory.resolve("eclair")))

    EclairBitcoindPair(eclair, bitcoind)
  }

  private[bitcoins] val binaryDirectory: Path =
    Paths.get(Properties.userHome, ".bitcoin-s", "binaries")
}
