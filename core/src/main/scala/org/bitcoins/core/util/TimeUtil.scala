package org.bitcoins.core.util

import java.time.Instant

object TimeUtil {

  def now: Instant = Instant.now

  /** Returns the current timestamp in milliseconds */
  def currentEpochMs: Long = now.toEpochMilli

  /** Returns the current timestamp in seconds */
  def currentEpochSecond: Long = {
    now.getEpochSecond
  }
}
