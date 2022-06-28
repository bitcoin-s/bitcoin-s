package org.bitcoins.core.util

import java.text.SimpleDateFormat
import java.time.format.DateTimeFormatter
import java.time.{Duration, Instant}
import java.util.concurrent.TimeUnit
import java.util.{Date, TimeZone}
import scala.concurrent.duration.FiniteDuration
import scala.util.{Failure, Success, Try}

object TimeUtil {

  def now: Instant = Instant.now

  /** Returns the current timestamp in milliseconds */
  def currentEpochMs: Long = now.toEpochMilli

  /** Returns the current timestamp in seconds */
  def currentEpochSecond: Long = {
    now.getEpochSecond
  }

  def iso8601ToInstant(str: String): Instant = {
    val ta = DateTimeFormatter.ISO_INSTANT.parse(str)
    Instant.from(ta)
  }

  def iso8601ToDate(str: String): Date = {
    val isoT = Try {
      val instant = iso8601ToInstant(str)
      Date.from(instant)
    }

    isoT match {
      case Success(date) => date
      case Failure(_)    =>
        // handle no time & timezone
        val dateFormat = new SimpleDateFormat("yyyy-MM-dd")
        dateFormat.setTimeZone(TimeZone.getTimeZone("UTC"))
        dateFormat.parse(str)
    }
  }

  def iso8601ToString(instant: Instant): String = {
    DateTimeFormatter.ISO_INSTANT.format(instant)
  }

  def iso8601ToString(date: Date): String = {
    DateTimeFormatter.ISO_INSTANT.format(date.toInstant)
  }

  def durationToFiniteDuration(duration: Duration): FiniteDuration = {
    FiniteDuration(duration.toNanos, TimeUnit.NANOSECONDS)
  }
}
