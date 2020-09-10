package org.bitcoins.core.util

/**
  * This StartStop trait will be used by methods that require broad start stop methods.
  * Provides structure for new clients to implement. For the async version please see
  * [[StartStopAsync]]
  * @tparam T
  */
trait StartStop[T] {
  def start(): T
  def stop(): T
}
