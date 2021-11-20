package org.bitcoins.lnd.rpc.config

import akka.actor.ActorSystem
import org.bitcoins.core.api.commons.InstanceFactoryLocal
import org.bitcoins.core.config._
import org.bitcoins.rpc.config.BitcoindAuthCredentials._
import org.bitcoins.rpc.config._
import scodec.bits._

import java.io.File
import java.net._
import java.nio.file._
import scala.util.Properties

sealed trait LndInstance {
  def rpcUri: URI
  def macaroon: String
  def certFileOpt: Option[File]
  def certificateOpt: Option[String]
}

case class LndInstanceLocal(
    datadir: Path,
    network: BitcoinNetwork,
    listenBinding: URI,
    restUri: URI,
    rpcUri: URI,
    bitcoindAuthCredentials: PasswordBased,
    bitcoindRpcUri: URI,
    zmqConfig: ZmqConfig,
    debugLevel: LogLevel)
    extends LndInstance {

  override val certificateOpt: Option[String] = None
  override val certFileOpt: Option[File] = Some(certFile)

  private var macaroonOpt: Option[String] = None

  override def macaroon: String = {
    macaroonOpt match {
      case Some(value) => value
      case None =>
        val path =
          datadir
            .resolve("data")
            .resolve("chain")
            .resolve("bitcoin")
            .resolve(LndInstanceLocal.getNetworkDirName(network))
            .resolve("admin.macaroon")

        val bytes = Files.readAllBytes(path)
        val hex = ByteVector(bytes).toHex

        macaroonOpt = Some(hex)
        hex
    }
  }

  lazy val certFile: File =
    datadir.resolve("tls.cert").toFile
}

object LndInstanceLocal
    extends InstanceFactoryLocal[LndInstanceLocal, ActorSystem] {

  override val DEFAULT_DATADIR: Path = Paths.get(Properties.userHome, ".lnd")

  override val DEFAULT_CONF_FILE: Path = DEFAULT_DATADIR.resolve("lnd.conf")

  private[lnd] def getNetworkDirName(network: BitcoinNetwork): String = {
    network match {
      case _: MainNet  => "mainnet"
      case _: TestNet3 => "testnet"
      case _: RegTest  => "regtest"
      case _: SigNet   => "signet"
    }
  }

  override def fromConfigFile(file: File = DEFAULT_CONF_FILE.toFile)(implicit
      system: ActorSystem): LndInstanceLocal = {
    require(file.exists, s"${file.getPath} does not exist!")
    require(file.isFile, s"${file.getPath} is not a file!")

    val config = LndConfig(file, file.getParentFile)

    fromConfig(config)
  }

  override def fromDataDir(dir: File = DEFAULT_DATADIR.toFile)(implicit
      system: ActorSystem): LndInstanceLocal = {
    require(dir.exists, s"${dir.getPath} does not exist!")
    require(dir.isDirectory, s"${dir.getPath} is not a directory!")

    val confFile = dir.toPath.resolve("lnd.conf").toFile
    val config = LndConfig(confFile, dir)

    fromConfig(config)
  }

  def fromConfig(config: LndConfig): LndInstanceLocal = {
    config.lndInstance
  }
}

case class LndInstanceRemote(
    rpcUri: URI,
    macaroon: String,
    certFileOpt: Option[File],
    certificateOpt: Option[String])
    extends LndInstance

object LndInstanceRemote {

  def apply(
      rpcUri: URI,
      macaroon: String,
      certFile: File): LndInstanceRemote = {
    LndInstanceRemote(rpcUri, macaroon, Some(certFile), None)
  }

  def apply(
      rpcUri: URI,
      macaroon: String,
      certificate: String): LndInstanceRemote = {
    LndInstanceRemote(rpcUri, macaroon, None, Some(certificate))
  }
}
