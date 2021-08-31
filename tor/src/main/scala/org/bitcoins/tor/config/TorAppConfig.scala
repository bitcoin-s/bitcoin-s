package org.bitcoins.tor.config

import com.typesafe.config.Config
import grizzled.slf4j.Logging
import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.commons.config.{AppConfig, AppConfigFactory, ConfigOps}
import org.bitcoins.commons.util.NativeProcessFactory
import org.bitcoins.core.util.{EnvUtil, NetworkUtil}
import org.bitcoins.tor.TorProtocolHandler.{Password, SafeCookie}
import org.bitcoins.tor.client.TorClient
import org.bitcoins.tor.{Socks5ProxyParams, TorParams}

import java.io.{File, FileNotFoundException}
import java.net.{InetAddress, InetSocketAddress}
import java.nio.file.{Files, Path, StandardCopyOption}
import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

/** Configuration for the Bitcoin-S node
  * @param directory The data directory of the node
  * @param confs Optional sequence of configuration overrides
  */
case class TorAppConfig(
    private val directory: Path,
    private val confs: Config*)(implicit
    override val executionContext: ExecutionContext)
    extends AppConfig
    with NativeProcessFactory
    with Logging {
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

  lazy val socks5ProxyParams: Socks5ProxyParams = {
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
      Socks5ProxyParams(
        address = address,
        credentialsOpt = None,
        randomizeCredentials = true
      )
    } else {
      val addr = new InetSocketAddress(InetAddress.getLoopbackAddress,
                                       Socks5ProxyParams.DefaultPort)
      Socks5ProxyParams(
        address = addr,
        credentialsOpt = None,
        randomizeCredentials = true
      )
    }
  }

  lazy val torParams: TorParams = {
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

      TorParams(address, auth, privKeyPath)
    } else {
      val control = new InetSocketAddress(InetAddress.getLoopbackAddress,
                                          TorParams.DefaultControlPort)

      val auth = SafeCookie()
      val privKeyPath = datadir.resolve("tor_priv_key")

      TorParams(control, auth, privKeyPath)
    }
  }

  lazy val enabled: Boolean = {
    //always enabled now ? Seems so because we always define the
    //torParams and socks5Params
    true
  }

  private val isBootstrappedLogLine = "Bootstrapped 100% (done): Done"

  def createClient(implicit ec: ExecutionContext): TorClient = {
    new TorClient()(ec, this)
  }

  /** Ensures correct tables and other required information is in
    * place for our node.
    */
  override def start(): Future[Unit] = {
    if (torProvided) {
      logger.info(s"Tor provided, ignoring TorAppConfig.start()")
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
        for {
          _ <- startBinary()
          _ = Runtime.getRuntime.addShutdownHook(new Thread() {
            override def run(): Unit = {
              // don't forget to stop the daemon on exit
              Await.result(stopBinary(), 30.seconds)
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
      stopBinary().map(_ => isStarted.set(false))
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
    val toCheck = socks5ProxyParams.address

    NetworkUtil.portIsBound(toCheck) && isAlive()
  }

  private lazy val authenticationArg: String =
    torParams.authentication match {
      case Password(_) =>
        //      s"--HashedControlPassword $password" // todo: need to hash the password correctly
        throw new RuntimeException("Password authentication not yet supported")
      case _: SafeCookie =>
        "--CookieAuthentication 1"
    }

  private lazy val executable = TorAppConfig.torBinaryFromResource(torDir)

  /** The command to start the daemon on the underlying OS */
  override lazy val cmd: String = {

    val args = Vector(
      "--ExitRelay 0", // ensure we aren't an exit relay
      "--BridgeRelay 0", // ensure we aren't an bridge relay
      s"--SOCKSPort ${socks5ProxyParams.address.getHostName}:${socks5ProxyParams.address.getPort}",
      s"--ControlPort ${torParams.controlAddress.getPort}",
      authenticationArg,
      s"""--DataDirectory "${torDir.toAbsolutePath}" """,
      s"""--Log "notice file ${torLogFile.toAbsolutePath}" """,
      s"""--GeoIPFile "${torDir.toAbsolutePath.resolve("geoip")}" """,
      s"""--GeoIPv6File "${torDir.toAbsolutePath.resolve("geoip6")}" """
    ).mkString(" ")

    s"$executable $args"
  }
}

object TorAppConfig extends AppConfigFactory[TorAppConfig] with Logging {

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

  /** Copies the tor executable and needed files to the given datadir
    * Returns the tor executable file
    * @param datadir Directory where to write files
    * @return Tor executable file
    */
  private def torBinaryFromResource(datadir: Path): File = {
    val torBundle = if (EnvUtil.isLinux) {
      linuxTorBundle
    } else if (EnvUtil.isMac) {
      osxTorBundle
    } else if (EnvUtil.isWindows) {
      windowsTorBundle
    } else throw new RuntimeException("Unknown platform")

    val executableFileName = datadir.resolve(torBundle.primaryExecutable).toFile

    logger.info(
      s"Using prepackaged Tor from bitcoin-s resources, $executableFileName")

    if (existsAndIsExecutable(datadir, torBundle)) {
      logger.info(
        s"Using tor daemon already written to datadir=${datadir.toAbsolutePath}")
      executableFileName
    } else {
      logger.info(
        s"Tor executable is not written to datadir $datadir, creating...")

      torBundle.allFilesNames.foreach { fileName =>
        val writePath = datadir.resolve(fileName)

        val parentDir = writePath.getParent.toFile

        if (!parentDir.exists()) {
          Files.createDirectories(parentDir.toPath)
        }

        writeFileFromResource(fileName, writePath)
      }

      //set files as executable
      torBundle.executables.foreach { f =>
        val executable = datadir.resolve(f)
        executable.toFile.setExecutable(true)
      }

      // write geoip files
      writeFileFromResource("geoip/geoip", datadir.resolve("geoip"))
      writeFileFromResource("geoip/geoip6", datadir.resolve("geoip6"))

      // write version file
      Files.write(datadir.resolve(TorClient.versionFileName),
                  TorClient.TOR_VERSION.getBytes)

      logger.info(
        s"Using prepackaged Tor from bitcoin-s resources, $executableFileName")

      executableFileName
    }
  }

  private def writeFileFromResource(
      resourceName: String,
      writePath: Path): Long = {
    val stream =
      Try(getClass.getResource("/" + resourceName).openStream()) match {
        case Failure(_)      => throw new FileNotFoundException(resourceName)
        case Success(stream) => stream
      }
    Files.copy(stream, writePath, StandardCopyOption.REPLACE_EXISTING)
  }

  /** The executables and lists of library files needed to run tor on a specific platform
    *
    * @param executables the files that need to be set to executable
    * @param fileList shared object files or library files for tor to operate
    */
  private case class TorFileBundle(
      executables: Vector[String],
      fileList: Vector[String]) {
    val allFilesNames: Vector[String] = executables ++ fileList

    /** By convention, make the primary executable the first element passed into executables
      * This is needed because some platforms like osx require two tor executables (tor, tor.real)
      */
    def primaryExecutable: String = executables.head
  }

  private lazy val linuxTorBundle: TorFileBundle = {
    TorFileBundle(
      executables = Vector("linux_64/tor", "linux_64/tor.real"),
      fileList = Vector(
        "linux_64/LICENSE",
        "linux_64/libssl.so.1.1",
        "linux_64/libevent-2.1.so.7",
        "linux_64/libcrypto.so.1.1",
        "linux_64/libstdc++/libstdc++.so.6"
      )
    )
  }

  private lazy val osxTorBundle: TorFileBundle = {
    TorFileBundle(
      executables = Vector(
        "osx_64/tor",
        "osx_64/tor.real"
      ),
      fileList = Vector("osx_64/LICENSE", "osx_64/libevent-2.1.7.dylib")
    )
  }

  private lazy val windowsTorBundle: TorFileBundle = {
    TorFileBundle(
      executables = Vector("windows_64/tor.exe"),
      fileList = Vector(
        "windows_64/libcrypto-1_1-x64.dll",
        "windows_64/libevent-2-1-7.dll",
        "windows_64/libevent_core-2-1-7.dll",
        "windows_64/libevent_extra-2-1-7.dll",
        "windows_64/libgcc_s_seh-1.dll",
        "windows_64/libssl-1_1-x64.dll",
        "windows_64/libssp-0.dll",
        "windows_64/libwinpthread-1.dll",
        "windows_64/LICENSE",
        "windows_64/zlib1.dll"
      )
    )
  }

  /** Checks if the executable files exists in the given datadir and are executable */
  private def existsAndIsExecutable(
      datadir: Path,
      bundle: TorFileBundle): Boolean = {

    val versionFile = datadir.resolve(TorClient.versionFileName)

    lazy val currentVersion = new String(Files.readAllBytes(versionFile))

    val latestVersion =
      versionFile.toFile.exists() && currentVersion == TorClient.TOR_VERSION

    lazy val hasFiles = bundle.executables.forall { executableFileName =>
      val executableFile = datadir.resolve(executableFileName).toFile
      executableFile.exists() && executableFile.canExecute
    }

    latestVersion && hasFiles
  }
}
