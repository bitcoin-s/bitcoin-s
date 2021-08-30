package org.bitcoins.tor.client

import grizzled.slf4j.Logging
import org.bitcoins.commons.util.NativeProcessFactory
import org.bitcoins.core.util.EnvUtil
import org.bitcoins.tor.config.TorAppConfig

import java.io.File
import scala.concurrent.ExecutionContext

/** A trait that helps start bitcoind/eclair when it is started via bitcoin-s */
class TorClient()(implicit
    val executionContext: ExecutionContext,
    conf: TorAppConfig)
    extends Logging {
  println(s"conf=$conf")
}

object TorClient extends Logging {

  // made by doing ./tor --version
  val TOR_VERSION = "Tor version 0.4.5.7 (git-83f895c015de5520)."
  val versionFileName = "version.txt"

  lazy val DEFAULT_TOR_LOCATION: Option[File] = {
    if (EnvUtil.isWindows) {
      NativeProcessFactory.findExecutableOnPath("tor.exe")
    } else {
      NativeProcessFactory.findExecutableOnPath("tor")
    }
  }
}
