package org.bitcoins.lnd.rpc.config

import grizzled.slf4j.Logging
import org.bitcoins.core.api.commons.ConfigFactory
import org.bitcoins.core.config._
import org.bitcoins.rpc.config.BitcoindAuthCredentials.PasswordBased
import org.bitcoins.rpc.config.ZmqConfig

import java.io.File
import java.net.{InetSocketAddress, URI}
import java.nio.file.{Files, Path, Paths}
import scala.util.Properties

/** This class represents a parsed `lnd.conf` file. It
  * respects the different ways of writing options in
  * `lnd.conf`: Raw options, network-prefixed options
  * and options within network sections. It also tries to
  * conform to the way lnd gives precedence to the
  * different properties.
  *
  * Not all options are exposed from this class. We only
  * expose those that are of relevance when making RPC
  * requests.
  */
case class LndConfig(private[bitcoins] val lines: Seq[String], datadir: File)
    extends Logging {

  //create datadir and config if it DNE on disk
  if (!datadir.exists()) {
    logger.debug(
      s"datadir=${datadir.getAbsolutePath} does not exist, creating now")
    datadir.mkdirs()
    LndConfig.writeConfigToFile(this, datadir)
  }

  private val confFile = datadir.toPath.resolve("lnd.conf")

  //create lnd.conf file in datadir if it does not exist
  if (!Files.exists(confFile)) {
    logger.debug(
      s"lnd.conf in datadir=${datadir.getAbsolutePath} does not exist, creating now")
    LndConfig.writeConfigToFile(this, datadir)
  }

  /** Converts the config back to a string that can be written
    * to file, and passed to `lnd`
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

  /** The blockchain network associated with this `lnd` config */
  lazy val network: BitcoinNetwork = {
    val isMainnet = getValue("bitcoin.mainnet").exists(_.toLowerCase == "true")
    val isTestnet = getValue("bitcoin.testnet").exists(_.toLowerCase == "true")
    val isSignet = getValue("bitcoin.simnet").exists(_.toLowerCase == "true")
    val isRegtest = getValue("bitcoin.regtest").exists(_.toLowerCase == "true")

    if (isMainnet && !(isRegtest || isSignet || isTestnet)) MainNet
    else if (isTestnet && !(isRegtest || isSignet || isMainnet)) TestNet3
    else if (isSignet && !(isRegtest || isTestnet || isMainnet)) SigNet
    else if (isRegtest && !(isSignet || isTestnet || isMainnet)) RegTest
    else
      throw new IllegalArgumentException("No network config set in lnd.conf")
  }

  private[config] def getValue(key: String): Option[String] = {
    val linesToSearchIn =
      lines.takeWhile(l => !l.trim.startsWith("[") || !l.trim.startsWith(";"))
    val collect = collectFrom(linesToSearchIn)(_)
    collect { case (`key`, value) =>
      value
    }.headOption
  }

  /** Converts a string to an InetSocketAddress */
  private def toInetSocketAddress(string: String): InetSocketAddress = {
    val uri = new URI(string)
    new InetSocketAddress(uri.getHost, uri.getPort)
  }

  lazy val bitcoindUser: String = getValue("bitcoind.rpcuser").get
  lazy val bitcoindPass: String = getValue("bitcoind.rpcpass").get

  lazy val listenBinding: URI = new URI({
    val baseUrl = getValue("listen").getOrElse("127.0.0.1:9735")
    if (baseUrl.startsWith("http")) baseUrl
    else "http://" + baseUrl
  })

  lazy val rpcBinding: URI = new URI({
    val baseUrl = getValue("rpclisten").getOrElse("127.0.0.1:10009")
    if (baseUrl.startsWith("http")) baseUrl
    else "http://" + baseUrl
  })

  lazy val restBinding: URI = new URI({
    val baseUrl = getValue("restlisten").getOrElse("127.0.0.1:8080")
    if (baseUrl.startsWith("http")) baseUrl
    else "http://" + baseUrl
  })

  lazy val bitcoindBinding: URI = new URI({
    val baseUrl =
      getValue("bitcoind.rpchost").getOrElse(s"127.0.0.1:${network.rpcPort}")
    if (baseUrl.startsWith("http")) baseUrl
    else "http://" + baseUrl
  })

  lazy val zmqpubrawblock: Option[InetSocketAddress] =
    getValue("bitcoind.zmqpubrawblock").map(toInetSocketAddress)

  lazy val zmqpubrawtx: Option[InetSocketAddress] =
    getValue("bitcoind.zmqpubrawtx").map(toInetSocketAddress)

  lazy val debuglevel: LogLevel = getValue("debuglevel")
    .flatMap(LogLevel.fromStringOpt)
    .getOrElse(LogLevel.Debug)

  /** Creates a new config with the given keys and values appended */
  def withOption(key: String, value: String): LndConfig = {
    val ourLines = this.lines
    val newLine = s"$key=$value"
    val lines = newLine +: ourLines
    val newConfig = LndConfig(lines, datadir)
    logger.debug(
      s"Appending new config with $key=$value to datadir=${datadir.getAbsolutePath}")
    LndConfig.writeConfigToFile(newConfig, datadir)

    newConfig
  }

  def withDatadir(newDatadir: File): LndConfig = {
    LndConfig(lines, newDatadir)
  }

  lazy val lndInstance: LndInstanceLocal = LndInstanceLocal(
    datadir.toPath,
    network,
    listenBinding,
    restBinding,
    rpcBinding,
    PasswordBased(bitcoindUser, bitcoindPass),
    bitcoindBinding,
    ZmqConfig(rawBlock = zmqpubrawblock, rawTx = zmqpubrawtx),
    debuglevel
  )
}

object LndConfig extends ConfigFactory[LndConfig] with Logging {

  /** The empty `lnd` config */
  override lazy val empty: LndConfig = LndConfig("", DEFAULT_DATADIR)

  /** Constructs a `lnd` config from the given string,
    * by splitting it on newlines
    */
  override def apply(config: String, datadir: File): LndConfig =
    apply(config.split("\n").toList, datadir)

  /** Reads the given path and construct a `lnd` config from it */
  override def apply(config: Path): LndConfig =
    apply(config.toFile, config.getParent.toFile)

  /** Reads the given file and construct a `lnd` config from it */
  override def apply(
      config: File,
      datadir: File = DEFAULT_DATADIR): LndConfig = {
    import org.bitcoins.core.compat.JavaConverters._
    val lines = Files
      .readAllLines(config.toPath)
      .iterator()
      .asScala
      .toList

    apply(lines, datadir)
  }

  override def fromConfigFile(file: File): LndConfig = {
    apply(file.toPath)
  }

  override def fromDataDir(dir: File): LndConfig = {
    apply(dir.toPath.resolve("lnd.conf"))
  }

  /** If there is a `lnd.conf` in the default
    * data directory, this is read. Otherwise, the
    * default configuration is returned.
    */
  override def fromDefaultDatadir: LndConfig = {
    if (DEFAULT_CONF_FILE.isFile) {
      apply(DEFAULT_CONF_FILE)
    } else {
      LndConfig.empty
    }
  }

  override val DEFAULT_DATADIR: File = {
    val path = if (Properties.isMac) {
      Paths.get(Properties.userHome, "Library", "Application Support", "Lnd")
    } else if (Properties.isWin) {
      Paths.get("C:", "Users", Properties.userName, "Appdata", "Local", "Lnd")
    } else {
      Paths.get(Properties.userHome, ".lnd")
    }
    path.toFile
  }

  /** Default location of lnd conf file */
  override val DEFAULT_CONF_FILE: File = DEFAULT_DATADIR.toPath
    .resolve("lnd.conf")
    .toFile

  /** Writes the config to the data directory within it, if it doesn't
    * exist. Returns the written file.
    */
  override def writeConfigToFile(config: LndConfig, datadir: File): Path = {

    val confStr = config.lines.mkString("\n")

    Files.createDirectories(datadir.toPath)
    val confFile = datadir.toPath.resolve("lnd.conf")

    if (datadir == DEFAULT_DATADIR && confFile == DEFAULT_CONF_FILE.toPath) {
      logger.warn(
        s"We will not overwrite the existing lnd.conf in default datadir")
    } else {
      Files.write(confFile, confStr.getBytes)
    }

    confFile
  }
}
