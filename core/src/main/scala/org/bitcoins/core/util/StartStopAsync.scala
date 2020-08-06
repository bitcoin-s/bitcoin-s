package org.bitcoins.core.util

import scala.concurrent.Future

/** Provide a uniform trait to start/stop a service asynchrously. For synchronous starts
  * and stops please see [[StartStop]] */
trait StartStopAsync[T] {
  def start(): Future[T]
  def stop(): Future[T]
}
