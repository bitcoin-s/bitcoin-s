package org.bitcoins.tor.config

import com.typesafe.config.Config
import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.commons.config.{AppConfig, AppConfigFactory, ConfigOps}
import org.bitcoins.core.util.NetworkUtil
import org.bitcoins.tor.TorProtocolHandler.{Password, SafeCookie}
import org.bitcoins.tor.client.TorClient
import org.bitcoins.tor.{Socks5ProxyParams, TorParams}

import java.io.File
import java.nio.file.{Files, Path}
import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, Future}

/** Configuration for the Bitcoin-S node
  * @param directory The data directory of the node
  * @param confs Optional sequence of configuration overrides
  */
case class TorAppConfig(
    private val directory: Path,
    private val confs: Config*)(implicit ec: ExecutionContext)
    extends AppConfig {
  override protected[bitcoins] def configOverrides: List[Config] = confs.toList
  override protected[bitcoins] def moduleName: String = TorAppConfig.moduleName
  override protected[bitcoins] type ConfigType = TorAppConfig

  override protected[bitcoins] def newConfigOfType(
      configs: Seq[Config]): TorAppConfig =
    TorAppConfig(directory, configs: _*)

  protected[bitcoins] def baseDatadir: Path = directory

  private val isStarted: AtomicBoolean = new AtomicBoolean(false)

  /** Ensures correct tables and other required information is in
    * place for our node.
    */
  override def start(): Future[Unit] = {
    if (torParams.isDefined && !isStarted.get) {
      isStarted.set(true)
      val start = System.currentTimeMillis()
      //remove old tor log file so we accurately tell when
      //the binary is started, if we don't remove this
      //we could have that log line appear from previous runs
      if (torLogFile.toFile.exists()) {
        torLogFile.toFile.delete()
      }
      val startedBinary: Future[Unit] = createClient.startBinary()
      for {
        _ <- startedBinary
        _ <- isBinaryFullyStarted()
      } yield {
        logger.info(
          s"Tor binary is fully started, it took=${System.currentTimeMillis() - start}ms")
      }
    } else if (isStarted.get) {
      logger.debug(s"Tor binary already started")
      Future.unit
    } else {
      logger.warn(
        s"Tor was requested to start, but it is diabled in the configuration file. Not starting tor")
      Future.unit
    }
  }

  private val isBootstrappedLogLine = "Bootstrapped 100% (done): Done"

  /** Checks if the tor binary is started by looking for a log in the [[torLogFile]]
    * The log line we are looking or is
    * {{{
    *     Bootstrapped 100% (done): Done
    *  }}}
    */
  private def isBinaryFullyStarted(): Future[Unit] = {
    AsyncUtil.retryUntilSatisfied(checkIfLogExists, 200.millis)
  }

  /** Checks it the [[isBootstrappedLogLine]] exists in the tor log file */
  private def checkIfLogExists: Boolean = {
    if (Files.exists(torLogFile)) {
      val stream = Files.lines(torLogFile)
      try {
        stream
          .filter((line: String) => line.contains(isBootstrappedLogLine))
          .count() > 0
      } finally if (stream != null) stream.close()
    } else {
      //log file may not exist quite yet as we just started the tor binary
      //it may take a bit for the tor binary to write the log file
      false
    }

  }

  override def stop(): Future[Unit] = {
    createClient
      .stopBinary()
      .map { _ =>
        isStarted.set(false)
        ()
      }
  }

  lazy val socks5ProxyParams: Option[Socks5ProxyParams] = {
    if (config.getBoolean("bitcoin-s.proxy.enabled")) {
      Some(
        Socks5ProxyParams(
          address = NetworkUtil.parseInetSocketAddress(
            config.getString("bitcoin-s.proxy.socks5"),
            Socks5ProxyParams.DefaultPort),
          credentialsOpt = None,
          randomizeCredentials = true
        )
      )
    } else {
      None
    }
  }

  lazy val torParams: Option[TorParams] = {
    if (config.getBoolean("bitcoin-s.tor.enabled")) {
      val control = NetworkUtil.parseInetSocketAddress(
        config.getString("bitcoin-s.tor.control"),
        TorParams.DefaultControlPort)

      val auth = config.getStringOrNone("bitcoin-s.tor.password") match {
        case Some(pass) => Password(pass)
        case None       => SafeCookie()
      }

      val privKeyPath =
        config.getStringOrNone("bitcoin-s.tor.privateKeyPath") match {
          case Some(path) => new File(path).toPath
          case None       => datadir.resolve("tor_priv_key")
        }

      Some(TorParams(control, auth, privKeyPath))
    } else {
      None
    }
  }

  lazy val enabled: Boolean = socks5ProxyParams.isDefined || torParams.isDefined

  def createClient(implicit ec: ExecutionContext): TorClient = {
    new TorClient()(ec, this)
  }

  lazy val torDir: Path = baseDatadir.resolve("tor")
  lazy val torLogFile: Path = torDir.resolve("TorLogs.txt")
}

object TorAppConfig extends AppConfigFactory[TorAppConfig] {

  override val moduleName: String = "tor"

  /** Constructs a tor configuration from the default Bitcoin-S
    * data directory and given list of configuration overrides.
    */
  override def fromDatadir(datadir: Path, confs: Vector[Config])(implicit
      ec: ExecutionContext): TorAppConfig =
    TorAppConfig(datadir, confs: _*)
}
