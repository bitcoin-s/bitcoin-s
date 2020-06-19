package org.bitcoins.core.util

import scala.concurrent.Future

/**
  * This StartStop trait will be used by methods that require broad start stop methods.
  * Provides structure for new clients to implement. Currently implemented by
  * BitcoindRpcClient and EclairRpcClient.
  * @tparam T
  */
trait StartStop[T] {
  def start(): Future[T]
  def stop(): Future[T]
}
