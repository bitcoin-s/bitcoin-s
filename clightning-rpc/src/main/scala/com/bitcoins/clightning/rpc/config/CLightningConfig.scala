package com.bitcoins.clightning.rpc.config

import grizzled.slf4j.Logging
import org.bitcoins.core.api.commons.ConfigFactory
import org.bitcoins.core.config._
import org.bitcoins.rpc.config.BitcoindAuthCredentials.PasswordBased

import java.io.File
import java.net.URI
import java.nio.file.{Files, Path, Paths}
import scala.util.Properties

/** This class represents a parsed `lightning.conf` file. It
  * respects the different ways of writing options in
  * `lightning.conf`: Raw options, network-prefixed options
  * and options within network sections. It also tries to
  * conform to the way clightning gives precedence to the
  * different properties.
  *
  * Not all options are exposed from this class. We only
  * expose those that are of relevance when making RPC
  * requests.
  */
case class CLightningConfig(
    private[bitcoins] val lines: Seq[String],
    datadir: File)
    extends Logging {

  //create datadir and config if it DNE on disk
  if (!datadir.exists()) {
    logger.debug(
      s"datadir=${datadir.getAbsolutePath} does not exist, creating now")
    datadir.mkdirs()
    CLightningConfig.writeConfigToFile(this, datadir)
  }

  private val confFile = datadir.toPath.resolve("config")

  //create config file in datadir if it does not exist
  if (!Files.exists(confFile)) {
    logger.debug(
      s"config in datadir=${datadir.getAbsolutePath} does not exist, creating now")
    CLightningConfig.writeConfigToFile(this, datadir)
  }

  /** Converts the config back to a string that can be written
    * to file, and passed to `lightning`
    */
  lazy val toWriteableString: String = lines.mkString("\n")

  /** Splits the provided lines into pairs of keys/values
    * based on `=`, and then applies the provided
    * `collect` function on those pairs
    */
  private def collectFrom(lines: Seq[String])(
      collect: PartialFunction[(String, String), String]): Seq[String] = {

    val splittedPairs = {
      val splitLines = lines.map(
        _.split("=")
          .map(_.trim)
          .toList)

      splitLines.collect { case h :: t :: _ =>
        h -> t
      }
    }

    splittedPairs.collect(collect)
  }

  /** The blockchain network associated with this `lightning` config */
  lazy val network: BitcoinNetwork =
    getValue("network").map(CLightningConfig.stringToNetwork).getOrElse(MainNet)

  private[config] def getValue(key: String): Option[String] = {
    val linesToSearchIn =
      lines.filter(l => !l.trim.startsWith("[") || !l.trim.startsWith("#"))
    val collect = collectFrom(linesToSearchIn)(_)
    collect { case (`key`, value) =>
      value
    }.headOption
  }

  lazy val bitcoindUser: String = getValue("bitcoin-rpcuser").get
  lazy val bitcoindPass: String = getValue("bitcoin-rpcpassword").get

  lazy val listenBinding: URI = new URI({
    val baseUrl = getValue("addr").getOrElse("127.0.0.1:9735")
    if (baseUrl.startsWith("tcp://")) baseUrl
    else "tcp://" + baseUrl
  })

  lazy val rpcFile: File = {
    val fromConfOpt = getValue("rpc-file").map(Paths.get(_).toFile)

    val netFolder = CLightningConfig.networkToConfigString(network)
    val default =
      datadir.toPath.resolve(netFolder).resolve("lightning-rpc")

    fromConfOpt.getOrElse(default.toFile)
  }

  lazy val logFileOpt: Option[File] = {
    getValue("log-file").map(Paths.get(_).toFile)
  }

  lazy val bitcoindBinding: URI = new URI({
    val baseUrl =
      getValue("bitcoin-rpcconnect").getOrElse("127.0.0.1")
    val port =
      getValue("bitcoin-rpcport").map(_.toInt).getOrElse(network.rpcPort)
    if (baseUrl.startsWith("http")) baseUrl + s":$port"
    else "http://" + baseUrl + s":$port"
  })

  /** Creates a new config with the given keys and values appended */
  def withOption(key: String, value: String): CLightningConfig = {
    val ourLines = this.lines
    val newLine = s"$key=$value"
    val lines = newLine +: ourLines
    val newConfig = CLightningConfig(lines, datadir)
    logger.debug(
      s"Appending new config with $key=$value to datadir=${datadir.getAbsolutePath}")
    CLightningConfig.writeConfigToFile(newConfig, datadir)

    newConfig
  }

  def withDatadir(newDatadir: File): CLightningConfig = {
    CLightningConfig(lines, newDatadir)
  }

  lazy val instance: CLightningInstanceLocal = CLightningInstanceLocal(
    datadir.toPath,
    network,
    rpcFile,
    listenBinding,
    logFileOpt,
    PasswordBased(bitcoindUser, bitcoindPass),
    bitcoindBinding
  )
}

object CLightningConfig extends ConfigFactory[CLightningConfig] with Logging {

  /** The empty `lightning` config */
  override lazy val empty: CLightningConfig =
    CLightningConfig("", DEFAULT_DATADIR)

  /** Constructs a `lightning` config from the given string,
    * by splitting it on newlines
    */
  override def apply(config: String, datadir: File): CLightningConfig =
    apply(config.split("\n").toList, datadir)

  /** Reads the given path and construct a `lightning` config from it */
  override def apply(config: Path): CLightningConfig =
    apply(config.toFile, config.getParent.toFile)

  /** Reads the given file and construct a `lightning` config from it */
  override def apply(
      config: File,
      datadir: File = DEFAULT_DATADIR): CLightningConfig = {
    import org.bitcoins.core.compat.JavaConverters._
    val lines = Files
      .readAllLines(config.toPath)
      .iterator()
      .asScala
      .toList

    apply(lines, datadir)
  }

  override def fromConfigFile(file: File): CLightningConfig = {
    apply(file.toPath)
  }

  override def fromDataDir(dir: File): CLightningConfig = {
    apply(dir.toPath.resolve("config"))
  }

  /** If there is a `config` in the default
    * data directory, this is read. Otherwise, the
    * default configuration is returned.
    */
  override def fromDefaultDatadir: CLightningConfig = {
    if (DEFAULT_CONF_FILE.isFile) {
      apply(DEFAULT_CONF_FILE)
    } else {
      CLightningConfig.empty
    }
  }

  override val DEFAULT_DATADIR: File = {
    val path = if (Properties.isMac) {
      Paths.get(Properties.userHome,
                "Library",
                "Application Support",
                "lightning")
    } else if (Properties.isWin) {
      Paths.get("C:",
                "Users",
                Properties.userName,
                "Appdata",
                "Local",
                "lightning")
    } else {
      Paths.get(Properties.userHome, ".lightning")
    }
    path.toFile
  }

  /** Default location of lightning conf file */
  override val DEFAULT_CONF_FILE: File = DEFAULT_DATADIR.toPath
    .resolve("config")
    .toFile

  /** Default location of lightning rpc file */
  val DEFAULT_RPC_FILE: File = DEFAULT_DATADIR.toPath
    .resolve("lightning-rpc")
    .toFile

  /** Writes the config to the data directory within it, if it doesn't
    * exist. Returns the written file.
    */
  override def writeConfigToFile(
      config: CLightningConfig,
      datadir: File): Path = {

    val confStr = config.lines.mkString("\n")

    Files.createDirectories(datadir.toPath)
    val confFile = datadir.toPath.resolve("config")

    if (datadir == DEFAULT_DATADIR && confFile == DEFAULT_CONF_FILE.toPath) {
      logger.warn(
        s"We will not overwrite the existing config in default datadir")
    } else {
      Files.write(confFile, confStr.getBytes)
    }

    confFile
  }

  def networkToConfigString(network: BitcoinNetwork): String = {
    network match {
      case MainNet  => "bitcoin"
      case TestNet3 => "testnet"
      case RegTest  => "regtest"
      case SigNet   => "signet"
    }
  }

  def stringToNetwork(string: String): BitcoinNetwork = {
    string match {
      case "bitcoin" => MainNet
      case "testnet" => TestNet3
      case "regtest" => RegTest
      case "signet"  => SigNet
      case str       => BitcoinNetworks.fromString(str)
    }
  }
}
