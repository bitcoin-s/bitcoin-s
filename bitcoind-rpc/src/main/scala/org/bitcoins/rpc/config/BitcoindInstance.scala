package org.bitcoins.rpc.config

import grizzled.slf4j.Logging
import org.bitcoins.core.api.commons.InstanceFactory
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.rpc.client.common.BitcoindVersion

import java.io.{File, FileNotFoundException}
import java.net.URI
import java.nio.file.{Files, Path, Paths}
import scala.sys.process._
import scala.util.Properties

/** Created by chris on 4/29/17.
  */
sealed trait BitcoindInstance extends Logging {

  require(binary.exists || isRemote,
          s"bitcoind binary path (${binary.getAbsolutePath}) does not exist!")

  // would like to check .canExecute as well, but we've run into issues on some machines
  require(binary.isFile || isRemote,
          s"bitcoind binary path (${binary.getAbsolutePath}) must be a file")

  /** The binary file that should get executed to start Bitcoin Core */
  def binary: File

  def datadir: File

  def network: NetworkParameters
  def uri: URI
  def rpcUri: URI
  def authCredentials: BitcoindAuthCredentials
  def zmqConfig: ZmqConfig

  def isRemote: Boolean

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

  def p2pPort: Int = uri.getPort
}

object BitcoindInstance extends InstanceFactory[BitcoindInstance] {

  private case class BitcoindInstanceImpl(
      network: NetworkParameters,
      uri: URI,
      rpcUri: URI,
      authCredentials: BitcoindAuthCredentials,
      zmqConfig: ZmqConfig,
      binary: File,
      datadir: File,
      isRemote: Boolean
  ) extends BitcoindInstance

  def apply(
      network: NetworkParameters,
      uri: URI,
      rpcUri: URI,
      authCredentials: BitcoindAuthCredentials,
      zmqConfig: ZmqConfig = ZmqConfig(),
      binary: File = DEFAULT_BITCOIND_LOCATION,
      datadir: File = BitcoindConfig.DEFAULT_DATADIR,
      isRemote: Boolean = false
  ): BitcoindInstance = {
    BitcoindInstanceImpl(network,
                         uri,
                         rpcUri,
                         authCredentials,
                         zmqConfig = zmqConfig,
                         binary = binary,
                         datadir = datadir,
                         isRemote = isRemote)
  }

  lazy val DEFAULT_BITCOIND_LOCATION: File = {

    def findExecutableOnPath(name: String): Option[File] =
      sys.env
        .getOrElse("PATH", "")
        .split(File.pathSeparator)
        .map(directory => new File(directory, name))
        .find(file => file.isFile && file.canExecute)

    val cmd =
      if (Properties.isWin) {
        findExecutableOnPath("bitcoind.exe")
      } else {
        findExecutableOnPath("bitcoind")
      }

    cmd.getOrElse(
      throw new FileNotFoundException("Cannot find a path to bitcoind"))
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
      binary: File = DEFAULT_BITCOIND_LOCATION
  ): BitcoindInstance = {
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

  override def fromDataDir(dir: File): BitcoindInstance = {
    fromDatadir(dir, DEFAULT_BITCOIND_LOCATION)
  }

  /** Construct a `bitcoind` from the given config file. If no `datadir` setting
    * is found, the parent directory to the given file is used.
    *
    * @throws  IllegalArgumentException if the given config file does not exist
    */
  def fromConfFile(
      file: File = BitcoindConfig.DEFAULT_CONF_FILE,
      binary: File = DEFAULT_BITCOIND_LOCATION
  ): BitcoindInstance = {
    require(file.exists, s"${file.getPath} does not exist!")
    require(file.isFile, s"${file.getPath} is not a file!")

    val conf = BitcoindConfig(file, file.getParentFile)

    fromConfig(conf, binary)
  }

  override def fromConfigFile(file: File): BitcoindInstance = {
    fromConfFile(file, DEFAULT_BITCOIND_LOCATION)
  }

  /** Constructs a `bitcoind` instance from the given config */
  def fromConfig(
      config: BitcoindConfig,
      binary: File = DEFAULT_BITCOIND_LOCATION
  ): BitcoindInstance = {

    val authCredentials = BitcoindAuthCredentials.fromConfig(config)
    BitcoindInstance(config.network,
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
