package org.bitcoins.core.api.asyncutil

import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration

trait AsyncUtilApi {

  /** Returns a future that completes after the given duration
    * This is useful for simulating a non blocking Thread.sleep()
    */
  def nonBlockingSleep(duration: FiniteDuration): Future[Unit]
}
