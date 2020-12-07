package org.bitcoins.testkit.util

import java.nio.file.{Path, Paths}

import scala.util.Properties

object TestkitBinaries {

  private val base: Path = Paths.get(".bitcoin-s", "binaries")

  /** The base directory where binaries needed in tests
    * are located.
    */
  lazy val baseBinaryDirectory: Path = {
    val home = Paths.get(Properties.userHome)
    fromRoot(home)
  }

  /** Gives you an arbitrary root path, and then tacks on .bitcoin-s/binaries/ onto the end of it
    */
  def fromRoot(path: Path): Path = {
    path.resolve(base)
  }
}
