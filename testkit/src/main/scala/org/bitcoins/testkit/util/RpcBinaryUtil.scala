package org.bitcoins.testkit.util

import org.bitcoins.core.config.RegTest
import org.bitcoins.core.util.StartStopAsync
import grizzled.slf4j.Logging

import java.nio.file.Path

/** A utility trait for handling binaries like bitcoind/eclair.
  * All common utility methods should go in this trait
  */
trait RpcBinaryUtil[T] extends StartStopAsync[T] with Logging {

  /** The path to the binary, an example is
    * '/home/suredbits/.bitcoin-s/binaries/bitcoind/bitcoin-0.20.1/bin/bitcoind'
    * or
    * '/home/suredbits/.bitcoin-s/binaries/eclair/0.4.1/eclair-node-0.4.1-e5fb281/bin/eclair-node.sh'
    */
  def binary: Path

  def binaryDirectory: Path = binary.getParent

  lazy val network = RegTest
}
