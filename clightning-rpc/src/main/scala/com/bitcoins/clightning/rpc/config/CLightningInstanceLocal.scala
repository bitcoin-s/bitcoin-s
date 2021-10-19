package com.bitcoins.clightning.rpc.config

import akka.actor.ActorSystem
import org.bitcoins.core.api.commons.InstanceFactoryLocal
import org.bitcoins.core.config._
import org.bitcoins.rpc.config.BitcoindAuthCredentials.PasswordBased

import java.io.File
import java.net.URI
import java.nio.file.{Path, Paths}
import scala.util.Properties

case class CLightningInstanceLocal(
    datadir: Path,
    network: BitcoinNetwork,
    rpcFile: File,
    listenBinding: URI,
    logFileOpt: Option[File],
    bitcoindAuthCredentials: PasswordBased,
    bitcoindRpcUri: URI)

object CLightningInstanceLocal
    extends InstanceFactoryLocal[CLightningInstanceLocal, ActorSystem] {

  override val DEFAULT_DATADIR: Path =
    Paths.get(Properties.userHome, ".lightning")

  override val DEFAULT_CONF_FILE: Path = DEFAULT_DATADIR.resolve("config")

  private[clightning] def getNetworkDirName(network: BitcoinNetwork): String = {
    network match {
      case MainNet  => ""
      case TestNet3 => "testnet"
      case SigNet   => "signet"
      case RegTest  => "regtest"
    }
  }

  override def fromConfigFile(file: File = DEFAULT_CONF_FILE.toFile)(implicit
      system: ActorSystem): CLightningInstanceLocal = {
    require(file.exists, s"${file.getPath} does not exist!")
    require(file.isFile, s"${file.getPath} is not a file!")

    val config = CLightningConfig(file, file.getParentFile)

    fromConfig(config)
  }

  override def fromDataDir(dir: File = DEFAULT_DATADIR.toFile)(implicit
      system: ActorSystem): CLightningInstanceLocal = {
    require(dir.exists, s"${dir.getPath} does not exist!")
    require(dir.isDirectory, s"${dir.getPath} is not a directory!")

    val confFile = dir.toPath.resolve("config").toFile
    val config = CLightningConfig(confFile, dir)

    fromConfig(config)
  }

  def fromConfig(config: CLightningConfig): CLightningInstanceLocal = {
    config.instance
  }
}
