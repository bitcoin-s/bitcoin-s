package org.bitcoins.testkit.util

import org.bitcoins.core.config.RegTest
import org.bitcoins.core.util.{BitcoinSLogger, StartStopAsync}

import java.nio.file.Path

/** A utility trait for handling binaries like bitcoind/eclair.
  * All common utility methods should go in this trait
  */
trait RpcBinaryUtil[T] extends StartStopAsync[T] with BitcoinSLogger {
  def binaryDirectory: Path

  lazy val network = RegTest
}
