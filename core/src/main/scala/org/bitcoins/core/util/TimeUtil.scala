package org.bitcoins.core.util

import java.time.Instant
import java.time.format.DateTimeFormatter
import java.util.Date

object TimeUtil {

  def now: Instant = Instant.now

  /** Returns the current timestamp in milliseconds */
  def currentEpochMs: Long = now.toEpochMilli

  /** Returns the current timestamp in seconds */
  def currentEpochSecond: Long = {
    now.getEpochSecond
  }

  def iso8601ToDate(str: String): Date = {
    val ta = DateTimeFormatter.ISO_INSTANT.parse(str)
    val instant = Instant.from(ta)
    Date.from(instant)
  }

  def iso8601ToString(date: Date): String = {
    DateTimeFormatter.ISO_INSTANT.format(date.toInstant)
  }
}
