package org.bitcoins.tor.client

import grizzled.slf4j.Logging
import org.bitcoins.core.util.EnvUtil
import org.bitcoins.tor.TorProtocolHandler._
import org.bitcoins.tor.config.TorAppConfig
import org.bitcoins.tor.{Socks5ProxyParams, TorParams}

import java.io.{File, FileNotFoundException}
import java.net.InetSocketAddress
import java.nio.file.{Files, Path, StandardCopyOption}
import scala.concurrent.{ExecutionContext, Future}
import scala.sys.process.{Process, ProcessBuilder}
import scala.util.{Failure, Success, Try}

/** A trait that helps start bitcoind/eclair when it is started via bitcoin-s */
class TorClient(implicit ec: ExecutionContext, conf: TorAppConfig)
    extends Logging {

  private[this] var processOpt: Option[Process] = None

  private lazy val process: ProcessBuilder = scala.sys.process.Process(cmd)

  lazy val socks5ProxyParams: Socks5ProxyParams = conf.socks5ProxyParams match {
    case Some(params) => params
    case None =>
      val addr = InetSocketAddress.createUnresolved(
        "127.0.0.1",
        Socks5ProxyParams.DefaultPort)
      Socks5ProxyParams(
        address = addr,
        credentialsOpt = None,
        randomizeCredentials = true
      )
  }

  lazy val torParams: TorParams = conf.torParams match {
    case Some(params) => params
    case None =>
      val control = InetSocketAddress.createUnresolved(
        "127.0.0.1",
        TorParams.DefaultControlPort)

      val auth = SafeCookie()
      val privKeyPath = conf.datadir.resolve("tor_priv_key")

      TorParams(control, auth, privKeyPath)
  }

  private lazy val authenticationArg = torParams.authentication match {
    case Password(_) =>
      "error"
//      s"--HashedControlPassword $password" // todo: need to hash the password correctly
    case _: SafeCookie =>
      "--CookieAuthentication 1"
  }

  /** The command to start the daemon on the underlying OS */
  lazy val cmd: String = {

    val args = Vector(
      "--ExitRelay 0", // ensure we aren't an exit relay
      "--BridgeRelay 0", // ensure we aren't an bridge relay
      s"--SOCKSPort ${socks5ProxyParams.address.getHostName}:${socks5ProxyParams.address.getPort}",
      s"--ControlPort ${torParams.controlAddress.getPort}",
      authenticationArg,
      s"""--DataDirectory "${conf.torDir.toAbsolutePath}" """,
      s"""--Log "notice file ${conf.torLogFile.toAbsolutePath}" """
    ).mkString(" ")

    val executable = TorClient.DEFAULT_TOR_LOCATION match {
      case Some(default) => default
      case None          => TorClient.torBinaryFromResource(conf.torDir)
    }

    s"$executable $args"
  }

  def isAlive: Boolean = {
    processOpt match {
      case Some(p) =>
        p.isAlive()
      case None =>
        false
    }
  }

  /** Starts the binary by spinning up a new process */
  def startBinary(): Future[Unit] = Future {
    processOpt match {
      case Some(_) =>
        //don't do anything as it is already started
        logger.info(s"Tor binary was already started!")
        ()
      case None =>
        if (cmd.nonEmpty) {
          val started = process.run()
          processOpt = Some(started)
        } else {
          logger.warn("cmd not set, no Tor binary started")
        }
        ()
    }
  }

  /** Stops the binary by destroying the underlying operating system process
    *
    * If the client is a remote client (not started on the host operating system)
    * this method is a no-op
    */
  def stopBinary(): Future[Unit] = Future {
    processOpt match {
      case Some(process) =>
        if (process.isAlive()) {
          val _ = process.destroy()
        }
        processOpt = None
      case None =>
        logger.info(s"No process found, binary wasn't started!")
        //no process running, nothing to do
        ()
    }
  }
}

object TorClient extends Logging {

  lazy val DEFAULT_TOR_LOCATION: Option[File] = {
    def findExecutableOnPath(name: String): Option[File] =
      sys.env
        .getOrElse("PATH", "")
        .split(File.pathSeparator)
        .map(directory => new File(directory, name))
        .find(file => file.isFile && file.canExecute)

    if (EnvUtil.isWindows) {
      findExecutableOnPath("tor.exe")
    } else {
      findExecutableOnPath("tor")
    }
  }

  /** Copies the tor executable and needed files to the given datadir
    * Returns the tor executable file
    * @param datadir Directory where to write files
    * @return Tor executable file
    */
  def torBinaryFromResource(datadir: Path): File = {
    // todo implement versioning
    val (torFileName, fileList) = if (EnvUtil.isLinux) {
      ("linux_64/tor", linuxFileList)
    } else if (EnvUtil.isMac) {
      ("osx_64/tor", osxFileList)
    } else if (EnvUtil.isWindows) {
      ("windows_64/tor.exe", windowsFileList)
    } else throw new RuntimeException("Unknown platform")

    val executableFileName = datadir.resolve(torFileName).toFile

    logger.info(
      s"Using prepackaged Tor from bitcoin-s resources, $executableFileName")

    if (executableFileName.exists() && executableFileName.canExecute) {
      executableFileName
    } else {
      logger.info("Tor executable is not written to datadir, creating...")

      fileList.foreach { fileName =>
        val stream =
          Try(getClass.getResource("/" + fileName).openStream()) match {
            case Failure(_)      => throw new FileNotFoundException(fileName)
            case Success(stream) => stream
          }

        val writePath = datadir.resolve(fileName)

        val parentDir = writePath.getParent.toFile

        if (!parentDir.exists()) {
          Files.createDirectories(parentDir.toPath)
        }

        Files.copy(stream, writePath, StandardCopyOption.REPLACE_EXISTING)
      }

      // set tor/tor.exe file as executable
      executableFileName.setExecutable(true)

      logger.info(
        s"Using prepackaged Tor from bitcoin-s resources, $executableFileName")

      executableFileName
    }
  }

  private lazy val linuxFileList: Vector[String] = {
    Vector(
      "linux_64/tor",
      "linux_64/LICENSE",
      "linux_64/libssl.so.1.1",
      "linux_64/libevent-2.1.so.7",
      "linux_64/libcrypto.so.1.1",
      "linux_64/libstdc++/libstdc++.so.6"
    )
  }

  private lazy val osxFileList: Vector[String] = {
    Vector(
      "osx_64/tor",
      "osx_64/tor.real",
      "osx_64/LICENSE",
      "osx_64/libevent-2.1.7.dylib"
    )
  }

  private lazy val windowsFileList: Vector[String] = {
    Vector(
      "windows_64/tor.exe",
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
  }
}
