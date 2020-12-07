package org.bitcoins.testkit.util

trait SbtBinaryFactory {

  /** The path where binaries are stored by sbt */
  def sbtBinaryDirectory: java.nio.file.Path
}
