package org.bitcoins.tor.client

import grizzled.slf4j.Logging
import org.bitcoins.commons.util.NativeProcessFactory
import org.bitcoins.core.util.EnvUtil
import org.bitcoins.tor.TorProtocolHandler._
import org.bitcoins.tor.config.TorAppConfig
import org.bitcoins.tor.{Socks5ProxyParams, TorParams}

import java.io.{File, FileNotFoundException}
import java.net.InetSocketAddress
import java.nio.file.{Files, Path, StandardCopyOption}
import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success, Try}

/** A trait that helps start bitcoind/eclair when it is started via bitcoin-s */
class TorClient()(implicit
    val executionContext: ExecutionContext,
    conf: TorAppConfig)
    extends NativeProcessFactory
    with Logging {

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
//      s"--HashedControlPassword $password" // todo: need to hash the password correctly
      throw new RuntimeException("Password authentication not yet supported")
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
}

object TorClient extends Logging {

  lazy val DEFAULT_TOR_LOCATION: Option[File] = {
    if (EnvUtil.isWindows) {
      NativeProcessFactory.findExecutableOnPath("tor.exe")
    } else {
      NativeProcessFactory.findExecutableOnPath("tor")
    }
  }

  /** Copies the tor executable and needed files to the given datadir
    * Returns the tor executable file
    * @param datadir Directory where to write files
    * @return Tor executable file
    */
  private def torBinaryFromResource(datadir: Path): File = {
    // todo implement versioning
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

      //set files as executable
      torBundle.executables.foreach { f =>
        val executable = datadir.resolve(f)
        executable.toFile.setExecutable(true)
      }

      logger.info(
        s"Using prepackaged Tor from bitcoin-s resources, $executableFileName")

      executableFileName
    }
  }

  /** The executables and lists of library files needed to run tor on a specific platform
    * @param executuables the files that need to be set to executable
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
      executables = Vector("linux_64/tor"),
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
    bundle.executables.forall { executableFileName =>
      val executableFile = datadir.resolve(executableFileName).toFile
      executableFile.exists() && executableFile.canExecute
    }
  }
}
