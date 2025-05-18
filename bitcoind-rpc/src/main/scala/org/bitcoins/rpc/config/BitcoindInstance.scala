package org.bitcoins.rpc.config

import com.typesafe.config.ConfigFactory
import org.apache.pekko.actor.ActorSystem
import org.bitcoins.commons.util.{BitcoinSLogger, NativeProcessFactory}
import org.bitcoins.core.api.commons.{InstanceFactory, InstanceFactoryLocal}
import org.bitcoins.core.api.tor.Socks5ProxyParams
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.rpc.client.common.BitcoindVersion

import java.io.File
import java.net.URI
import java.nio.file.{Files, Path, Paths}
import scala.sys.process._
import scala.util.{Failure, Properties, Success, Try}

/** Created by chris on 4/29/17.
  */
sealed trait BitcoindInstance extends BitcoinSLogger {

  def network: NetworkParameters
  def uri: URI
  def rpcUri: URI
  def authCredentials: BitcoindAuthCredentials
  def zmqConfig: ZmqConfig

  def p2pPort: Int = uri.getPort
  implicit def system: ActorSystem

  def bitcoindRpcAppConfig: BitcoindRpcAppConfig

}

/** Represents a bitcoind instance that is running locally on the same host */
sealed trait BitcoindInstanceLocal extends BitcoindInstance {

  /** The binary file that should get executed to start Bitcoin Core */
  def binary: File

  def datadir: File

  // would like to check .canExecute as well, but we've run into issues on some machines
  require(
    binary.isFile,
    s"bitcoind binary path (${binary.getAbsolutePath}) must be a file"
  )

  require(
    binary.exists,
    s"bitcoind binary path (${binary.getAbsolutePath}) does not exist!"
  )

  def getVersion: BitcoindVersion = {

    val binaryPath = binary.getAbsolutePath
    val versionT = Try {
      val cmd =
        Seq(binaryPath, s"-datadir=${datadir.toPath.toString}", "--version")
      val foundVersion =
        cmd
          .!!(NativeProcessFactory.processLogger)
          .split(Properties.lineSeparator)
          .head
          .split(" ")
          .last
      BitcoindVersion.findVersion(foundVersion).getOrElse {
        // else just default to the newest version of bitcoind
        logger.warn(
          s"Unsupported Bitcoin Core version: $foundVersion. The latest supported version is ${BitcoindVersion.newest}"
        )
        BitcoindVersion.newest
      }
    }

    versionT match {
      case Success(value) => value
      case Failure(exception) =>
        logger.error("Error getting bitcoind version", exception)
        throw exception
    }
  }
}

/** Refers to a bitcoind instance that is running remotely on another machine */
sealed trait BitcoindInstanceRemote extends BitcoindInstance {
  def proxyParams: Option[Socks5ProxyParams]
}

object BitcoindInstanceLocal
    extends InstanceFactoryLocal[BitcoindInstanceLocal, ActorSystem] {

  private case class BitcoindInstanceLocalImpl(
      network: NetworkParameters,
      uri: URI,
      rpcUri: URI,
      authCredentials: BitcoindAuthCredentials,
      zmqConfig: ZmqConfig,
      binary: File,
      datadir: File
  )(
      implicit override val system: ActorSystem,
      implicit override val bitcoindRpcAppConfig: BitcoindRpcAppConfig)
      extends BitcoindInstanceLocal

  def apply(
      network: NetworkParameters,
      uri: URI,
      rpcUri: URI,
      authCredentials: BitcoindAuthCredentials,
      zmqConfig: ZmqConfig = ZmqConfig(),
      binary: File = DEFAULT_BITCOIND_LOCATION match {
        case Some(file) => file
        case None       => bitcoindLocationFromConfigFile
      }
  )(implicit
      system: ActorSystem,
      bitcoindRpcAppConfig: BitcoindRpcAppConfig): BitcoindInstanceLocal = {
    BitcoindInstanceLocalImpl(
      network,
      uri,
      rpcUri,
      authCredentials,
      zmqConfig = zmqConfig,
      binary = binary,
      datadir = bitcoindRpcAppConfig.datadir.toFile
    )(system, bitcoindRpcAppConfig)
  }

  lazy val DEFAULT_BITCOIND_LOCATION: Option[File] = {
    val cmd =
      if (Properties.isWin) {
        NativeProcessFactory.findExecutableOnPath("bitcoind.exe")
      } else {
        NativeProcessFactory.findExecutableOnPath("bitcoind")
      }

    cmd
  }

  lazy val remoteFilePath: File = {
    Files.createTempFile("dummy", "").toFile
  }

  def bitcoindLocationFromConfigFile: File = {
    val homeVar = sys.env("HOME");
    val config = ConfigFactory
      .parseFile(new File(homeVar + "/.bitcoin-s/bitcoin-s.conf"))
      .resolve()
    new File(config.getString("bitcoin-s.bitcoind-rpc.binary"))
  }

  /** Constructs a `bitcoind` instance from the given datadir, using the
    * `bitcoin.conf` found within (if any)
    *
    * @throws IllegalArgumentException
    *   if the given datadir does not exist
    */
  def fromDatadir(
      datadir: File = BitcoindConfig.DEFAULT_DATADIR,
      binary: File = DEFAULT_BITCOIND_LOCATION match {
        case Some(file) => file
        case None       => bitcoindLocationFromConfigFile
      }
  )(implicit system: ActorSystem): BitcoindInstanceLocal = {
    require(datadir.exists, s"${datadir.getPath} does not exist!")
    require(datadir.isDirectory, s"${datadir.getPath} is not a directory!")

    val configPath = Paths.get(datadir.getAbsolutePath, "bitcoin.conf")
    if (Files.exists(configPath)) {

      val file = configPath.toFile()
      fromConfFile(file, binary)
    } else {
      fromConfig(BitcoindConfig.empty, binary)
    }
  }

  def fromDataDir(
      dir: File
  )(implicit system: ActorSystem): BitcoindInstanceLocal = {
    fromDatadir(
      dir,
      DEFAULT_BITCOIND_LOCATION match {
        case Some(file) => file
        case None       => bitcoindLocationFromConfigFile
      }
    )
  }

  /** Construct a `bitcoind` from the given config file. If no `datadir` setting
    * is found, the parent directory to the given file is used.
    *
    * @throws IllegalArgumentException
    *   if the given config file does not exist
    */
  def fromConfFile(
      file: File = BitcoindConfig.DEFAULT_CONF_FILE,
      binary: File = DEFAULT_BITCOIND_LOCATION match {
        case Some(f) => f
        case None    => bitcoindLocationFromConfigFile
      }
  )(implicit system: ActorSystem): BitcoindInstanceLocal = {
    require(file.exists, s"${file.getPath} does not exist!")
    require(file.isFile, s"${file.getPath} is not a file!")

    val conf = BitcoindConfig(file, file.getParentFile)

    fromConfig(conf, binary)
  }

  def fromConfigFile(
      file: File
  )(implicit system: ActorSystem): BitcoindInstanceLocal = {
    fromConfFile(
      file,
      DEFAULT_BITCOIND_LOCATION match {
        case Some(f) => f
        case None    => bitcoindLocationFromConfigFile
      }
    )
  }

  /** Constructs a `bitcoind` instance from the given config */
  def fromConfig(
      config: BitcoindConfig,
      binary: File = DEFAULT_BITCOIND_LOCATION match {
        case Some(file) => file
        case None       => bitcoindLocationFromConfigFile
      }
  )(implicit system: ActorSystem): BitcoindInstanceLocal = {

    val authCredentials = BitcoindAuthCredentials.fromConfig(config)
    val bitcoindRpcAppConfig =
      BitcoindRpcAppConfig.fromDatadir(config.datadir.toPath)
    BitcoindInstanceLocalImpl(
      config.network,
      config.uri,
      config.rpcUri,
      authCredentials,
      zmqConfig = ZmqConfig.fromConfig(config),
      binary = binary,
      datadir = config.datadir
    )(system, bitcoindRpcAppConfig)
  }

  override val DEFAULT_DATADIR: Path = BitcoindConfig.DEFAULT_DATADIR.toPath

  override val DEFAULT_CONF_FILE: Path = BitcoindConfig.DEFAULT_CONF_FILE.toPath
}

object BitcoindInstanceRemote
    extends InstanceFactory[BitcoindInstanceRemote, ActorSystem] {

  private case class BitcoindInstanceRemoteImpl(
      network: NetworkParameters,
      uri: URI,
      rpcUri: URI,
      authCredentials: BitcoindAuthCredentials,
      zmqConfig: ZmqConfig,
      proxyParams: Option[Socks5ProxyParams]
  )(
      implicit override val system: ActorSystem,
      implicit override val bitcoindRpcAppConfig: BitcoindRpcAppConfig)
      extends BitcoindInstanceRemote

  def apply(
      network: NetworkParameters,
      uri: URI,
      rpcUri: URI,
      authCredentials: BitcoindAuthCredentials,
      zmqConfig: ZmqConfig = ZmqConfig(),
      proxyParams: Option[Socks5ProxyParams] = None
  )(implicit
      system: ActorSystem,
      bitcoindRpcAppConfig: BitcoindRpcAppConfig): BitcoindInstanceRemote = {
    BitcoindInstanceRemoteImpl(
      network,
      uri,
      rpcUri,
      authCredentials,
      zmqConfig = zmqConfig,
      proxyParams = proxyParams
    )
  }

  def fromConfFile(
      file: File
  )(implicit system: ActorSystem): BitcoindInstanceRemote = {
    require(file.exists, s"${file.getPath} does not exist!")
    require(file.isFile, s"${file.getPath} is not a file!")

    val conf = BitcoindRpcAppConfig(file.toPath, Vector.empty)
    fromConfig(conf)
  }

  override def fromConfigFile(
      file: File
  )(implicit system: ActorSystem): BitcoindInstanceRemote = {
    fromConfFile(
      file
    )
  }

  def fromConfig(
      config: BitcoindRpcAppConfig
  )(implicit system: ActorSystem): BitcoindInstanceRemote = {

    BitcoindInstanceRemoteImpl(
      config.network,
      config.uri,
      config.rpcUri,
      config.authCredentials,
      zmqConfig = config.zmqConfig,
      proxyParams = None
    )(system, config)
  }

  override def fromDataDir(
      dir: File
  )(implicit system: ActorSystem): BitcoindInstanceRemote = {
    require(dir.exists, s"${dir.getPath} does not exist!")
    require(dir.isDirectory, s"${dir.getPath} is not a directory!")
    val conf = BitcoindRpcAppConfig(dir.toPath, Vector.empty)
    fromConfig(conf)
  }
}
