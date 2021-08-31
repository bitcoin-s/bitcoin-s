package org.bitcoins.tor.config

import com.typesafe.config.Config
import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.commons.config.{AppConfig, AppConfigFactory, ConfigOps}
import org.bitcoins.core.util.NetworkUtil
import org.bitcoins.tor.TorProtocolHandler.{Password, SafeCookie}
import org.bitcoins.tor.client.TorClient
import org.bitcoins.tor.{Socks5ProxyParams, TorParams}

import java.io.File
import java.net.{InetAddress, InetSocketAddress}
import java.nio.file.{Files, Path}
import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Future}

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

  lazy val torDir: Path = baseDatadir.resolve("tor")

  lazy val torLogFile: Path = torDir.resolve("TorLogs.txt")

  lazy val torProvided = config.getBoolean("bitcoin-s.tor.provided")

  lazy val useRandomPorts = config.getBoolean("bitcoin-s.tor.use-random-ports")

  lazy val socks5ProxyParams: Option[Socks5ProxyParams] = {
    if (config.getBoolean("bitcoin-s.proxy.enabled")) {
      val address = if (torProvided) {
        NetworkUtil.parseInetSocketAddress(
          config.getString("bitcoin-s.proxy.socks5"),
          TorParams.DefaultProxyPort)
      } else {
        new InetSocketAddress(InetAddress.getLoopbackAddress,
                              if (useRandomPorts)
                                TorAppConfig.randomSocks5Port
                              else TorParams.DefaultProxyPort)
      }
      Some(
        Socks5ProxyParams(
          address = address,
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
      val address = if (torProvided) {
        NetworkUtil.parseInetSocketAddress(
          config.getString("bitcoin-s.tor.control"),
          TorParams.DefaultControlPort)
      } else {
        new InetSocketAddress(InetAddress.getLoopbackAddress,
                              if (useRandomPorts)
                                TorAppConfig.randomControlPort
                              else TorParams.DefaultControlPort)
      }

      val auth = config.getStringOrNone("bitcoin-s.tor.password") match {
        case Some(pass) => Password(pass)
        case None       => SafeCookie()
      }

      val privKeyPath =
        config.getStringOrNone("bitcoin-s.tor.privateKeyPath") match {
          case Some(path) => new File(path).toPath
          case None       => datadir.resolve("tor_priv_key")
        }

      Some(TorParams(address, auth, privKeyPath))
    } else {
      None
    }
  }

  lazy val enabled: Boolean = socks5ProxyParams.isDefined || torParams.isDefined

  private val isBootstrappedLogLine = "Bootstrapped 100% (done): Done"

  def createClient(implicit ec: ExecutionContext): TorClient = {
    new TorClient()(ec, this)
  }

  /** Ensures correct tables and other required information is in
    * place for our node.
    */
  override def start(): Future[Unit] = {
    if (torProvided) {
      Future.unit
    } else {
      lazy val torRunning = checkIfTorAlreadyRunning
      if (enabled && !isStarted.get && !torRunning) {
        isStarted.set(true)
        logger.info(s"Starting Tor daemon")
        val start = System.currentTimeMillis()
        //remove old tor log file so we accurately tell when
        //the binary is started, if we don't remove this
        //we could have that log line appear from previous runs
        if (torLogFile.toFile.exists()) {
          torLogFile.toFile.delete()
        }
        val client = createClient
        for {
          _ <- client.startBinary()
          _ = Runtime.getRuntime.addShutdownHook(new Thread() {
            override def run(): Unit = {
              // don't forget to stop the daemon on exit
              Await.result(client.stopBinary(), 30.seconds)
            }
          })
          _ <- isBinaryFullyStarted()
        } yield {
          logger.info(
            s"Tor daemon is fully started, it took=${System.currentTimeMillis() - start}ms")
        }
      } else if (isStarted.get) {
        logger.debug(s"Tor daemon already started")
        Future.unit
      } else if (torRunning) {
        logger.warn(
          s"Tor daemon was requested to start, but it is already running. Not starting tor")
        Future.unit
      } else {
        logger.warn(
          s"Tor daemon was requested to start, but it is disabled in the configuration file. Not starting tor")
        Future.unit
      }
    }
  }

  override def stop(): Future[Unit] = {
    if (torProvided) {
      Future.unit
    } else {
      createClient.stopBinary().map(_ => isStarted.set(false))
    }
  }

  /** Checks if the tor binary is started by looking for a log in the [[torLogFile]]
    * The log line we are looking or is
    * {{{
    *     Bootstrapped 100% (done): Done
    *  }}}
    */
  private def isBinaryFullyStarted(): Future[Unit] = {
    //tor can take at least 25 seconds to start at times
    //see: https://github.com/bitcoin-s/bitcoin-s/pull/3558#issuecomment-899819698
    AsyncUtil.retryUntilSatisfied(checkIfLogExists, 1.second, 60)
  }

  /** Checks it the [[isBootstrappedLogLine]] exists in the tor log file */
  private def checkIfLogExists: Boolean = {
    torLogFile.toFile.exists() && {
      val stream = Files.lines(torLogFile)
      try {
        stream
          .filter((line: String) => line.contains(isBootstrappedLogLine))
          .count() > 0
      } finally if (stream != null) stream.close()
    }
  }

  private def checkIfTorAlreadyRunning: Boolean = {
    val toCheck = socks5ProxyParams match {
      case Some(params) => params.address
      case None =>
        torParams match {
          case Some(params) => params.controlAddress
          case None =>
            new InetSocketAddress(InetAddress.getLoopbackAddress,
                                  TorParams.DefaultProxyPort)
        }
    }

    NetworkUtil.portIsBound(toCheck)
  }
}

object TorAppConfig extends AppConfigFactory[TorAppConfig] {

  override val moduleName: String = "tor"

  /** Constructs a tor configuration from the default Bitcoin-S
    * data directory and given list of configuration overrides.
    */
  override def fromDatadir(datadir: Path, confs: Vector[Config])(implicit
      ec: ExecutionContext): TorAppConfig =
    TorAppConfig(datadir, confs: _*)

  lazy val randomSocks5Port: Int = ports.proxyPort

  lazy val randomControlPort: Int = ports.controlPort

  private case class TorPorts(proxyPort: Int, controlPort: Int)

  private lazy val ports = {
    val proxyPort = NetworkUtil.randomPort()

    def findControlPort: Int = {
      1.to(1024).foreach { _ =>
        val controlPort = NetworkUtil.randomPort()
        if (proxyPort != controlPort) {
          return controlPort
        }
      }
      throw new RuntimeException("Cannot find a non-bound port")
    }

    TorPorts(proxyPort, findControlPort)
  }

}
