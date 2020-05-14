package org.bitcoins.core.util

import scala.util.Properties

object EnvUtil {
  private val osName = System.getProperty("os.name")

  lazy val isLinux: Boolean = osName.startsWith("Linux")

  lazy val isMac: Boolean = osName.startsWith("Mac")

  lazy val isWindows: Boolean = osName.startsWith("Windows")

  lazy val isCI: Boolean = Properties.envOrNone("CI").contains("1")
}
