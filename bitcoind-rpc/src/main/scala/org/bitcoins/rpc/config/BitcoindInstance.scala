package org.bitcoins.rpc.config

import com.typesafe.config.ConfigFactory
import grizzled.slf4j.Logging
import org.bitcoins.commons.util.NativeProcessFactory
import org.bitcoins.core.api.commons.{InstanceFactory, InstanceFactoryLocal}
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.rpc.client.common.BitcoindVersion
import org.bitcoins.tor.Socks5ProxyParams

import java.io.File
import java.net.URI
import java.nio.file.{Files, Path, Paths}
import scala.sys.process._
import scala.util.Properties

/** Created by chris on 4/29/17.
  */
sealed trait BitcoindInstance extends Logging {

  def network: NetworkParameters
  def uri: URI
  def rpcUri: URI
  def authCredentials: BitcoindAuthCredentials
  def zmqConfig: ZmqConfig

  def p2pPort: Int = uri.getPort
}

/** Represents a bitcoind instance that is running locally on the same host */
sealed trait BitcoindInstanceLocal extends BitcoindInstance {

  /** The binary file that should get executed to start Bitcoin Core */
  def binary: File

  def datadir: File

  // would like to check .canExecute as well, but we've run into issues on some machines
  require(binary.isFile,
          s"bitcoind binary path (${binary.getAbsolutePath}) must be a file")

  require(binary.exists,
          s"bitcoind binary path (${binary.getAbsolutePath}) does not exist!")

  def getVersion: BitcoindVersion = {

    val binaryPath = binary.getAbsolutePath

    val foundVersion =
      Seq(binaryPath, "--version").!!.split(Properties.lineSeparator).head
        .split(" ")
        .last

    foundVersion match {
      case _: String
          if foundVersion.equals(BitcoindVersion.Experimental.toString) =>
        BitcoindVersion.Experimental
      case _: String if foundVersion.startsWith(BitcoindVersion.V16.toString) =>
        BitcoindVersion.V16
      case _: String if foundVersion.startsWith(BitcoindVersion.V17.toString) =>
        BitcoindVersion.V17
      case _: String if foundVersion.startsWith(BitcoindVersion.V18.toString) =>
        BitcoindVersion.V18
      case _: String if foundVersion.startsWith(BitcoindVersion.V19.toString) =>
        BitcoindVersion.V19
      case _: String if foundVersion.startsWith(BitcoindVersion.V20.toString) =>
        BitcoindVersion.V20
      case _: String if foundVersion.startsWith(BitcoindVersion.V21.toString) =>
        BitcoindVersion.V21
      case _: String => BitcoindVersion.Unknown
    }
  }
}

/** Refers to a bitcoind instance that is running remotely on another machine */
sealed trait BitcoindInstanceRemote extends BitcoindInstance {
  def proxyParams: Option[Socks5ProxyParams]
}

object BitcoindInstanceLocal
    extends InstanceFactoryLocal[BitcoindInstanceLocal] {

  private case class BitcoindInstanceLocalImpl(
      network: NetworkParameters,
      uri: URI,
      rpcUri: URI,
      authCredentials: BitcoindAuthCredentials,
      zmqConfig: ZmqConfig,
      binary: File,
      datadir: File
  ) extends BitcoindInstanceLocal

  def apply(
      network: NetworkParameters,
      uri: URI,
      rpcUri: URI,
      authCredentials: BitcoindAuthCredentials,
      zmqConfig: ZmqConfig = ZmqConfig(),
      binary: File = DEFAULT_BITCOIND_LOCATION match {
        case Some(file) => file
        case None => {
          val homeVar = sys.env("HOME");
          val config = ConfigFactory
            .parseFile(new File(homeVar + "/.bitcoin-s/bitcoin-s.conf"))
            .resolve()
          new File(config.getString("bitcoin-s.bitcoind-rpc.binary"))
        }
      },
      datadir: File = BitcoindConfig.DEFAULT_DATADIR
  ): BitcoindInstanceLocal = {
    BitcoindInstanceLocalImpl(network,
                              uri,
                              rpcUri,
                              authCredentials,
                              zmqConfig = zmqConfig,
                              binary = binary,
                              datadir = datadir)
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

  /** Constructs a `bitcoind` instance from the given datadir, using the
    * `bitcoin.conf` found within (if any)
    *
    * @throws IllegalArgumentException if the given datadir does not exist
    */
  def fromDatadir(
      datadir: File = BitcoindConfig.DEFAULT_DATADIR,
      binary: File = DEFAULT_BITCOIND_LOCATION match {
        case Some(file) => file
        case None => {
          val homeVar = sys.env("HOME");
          val config = ConfigFactory
            .parseFile(new File(homeVar + "/.bitcoin-s/bitcoin-s.conf"))
            .resolve()
          new File(config.getString("bitcoin-s.bitcoind-rpc.binary"))
        }
      }
  ): BitcoindInstanceLocal = {
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

  override def fromDataDir(dir: File): BitcoindInstanceLocal = {
    fromDatadir(
      dir,
      DEFAULT_BITCOIND_LOCATION match {
        case Some(file) => file
        case None => {
          val homeVar = sys.env("HOME");
          val config = ConfigFactory
            .parseFile(new File(homeVar + "/.bitcoin-s/bitcoin-s.conf"))
            .resolve()
          new File(config.getString("bitcoin-s.bitcoind-rpc.binary"))
        }
      }
    )
  }

  /** Construct a `bitcoind` from the given config file. If no `datadir` setting
    * is found, the parent directory to the given file is used.
    *
    * @throws  IllegalArgumentException if the given config file does not exist
    */
  def fromConfFile(
      file: File = BitcoindConfig.DEFAULT_CONF_FILE,
      binary: File = DEFAULT_BITCOIND_LOCATION match {
        case Some(file) => file
        case None => {
          val homeVar = sys.env("HOME");
          val config = ConfigFactory
            .parseFile(new File(homeVar + "/.bitcoin-s/bitcoin-s.conf"))
            .resolve()
          new File(config.getString("bitcoin-s.bitcoind-rpc.binary"))
        }
      }
  ): BitcoindInstanceLocal = {
    require(file.exists, s"${file.getPath} does not exist!")
    require(file.isFile, s"${file.getPath} is not a file!")

    val conf = BitcoindConfig(file, file.getParentFile)

    fromConfig(conf, binary)
  }

  override def fromConfigFile(file: File): BitcoindInstanceLocal = {
    fromConfFile(
      file,
      DEFAULT_BITCOIND_LOCATION match {
        case Some(file) => file
        case None => {
          val homeVar = sys.env("HOME");
          val config = ConfigFactory
            .parseFile(new File(homeVar + "/.bitcoin-s/bitcoin-s.conf"))
            .resolve()
          new File(config.getString("bitcoin-s.bitcoind-rpc.binary"))
        }
      }
    )
  }

  /** Constructs a `bitcoind` instance from the given config */
  def fromConfig(
      config: BitcoindConfig,
      binary: File = DEFAULT_BITCOIND_LOCATION match {
        case Some(file) => file
        case None => {
          val homeVar = sys.env("HOME");
          val config = ConfigFactory
            .parseFile(new File(homeVar + "/.bitcoin-s/bitcoin-s.conf"))
            .resolve()
          new File(config.getString("bitcoin-s.bitcoind-rpc.binary"))
        }
      }
  ): BitcoindInstanceLocal = {

    val authCredentials = BitcoindAuthCredentials.fromConfig(config)
    BitcoindInstanceLocalImpl(config.network,
                              config.uri,
                              config.rpcUri,
                              authCredentials,
                              zmqConfig = ZmqConfig.fromConfig(config),
                              binary = binary,
                              datadir = config.datadir)
  }

  override val DEFAULT_DATADIR: Path = BitcoindConfig.DEFAULT_DATADIR.toPath

  override val DEFAULT_CONF_FILE: Path = BitcoindConfig.DEFAULT_CONF_FILE.toPath
}

object BitcoindInstanceRemote1 extends InstanceFactory[BitcoindInstanceRemote] {
  override def fromConfigFile(file: File): BitcoindInstanceRemote = ???

  override def fromDataDir(dir: File): BitcoindInstanceRemote = ???
}
