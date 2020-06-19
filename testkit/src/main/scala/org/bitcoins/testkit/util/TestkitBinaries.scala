package org.bitcoins.testkit.util

import java.nio.file.{Path, Paths}

import scala.util.Properties

object TestkitBinaries {

  /** The base directory where binaries needed in tests
    * are located.
    */
  lazy val baseBinaryDirectory: Path = {
    val home = Paths.get(Properties.userHome, ".bitcoin-s", "binaries")
    home
  }

}
