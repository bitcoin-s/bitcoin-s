package org.bitcoins.core.util

import java.text.SimpleDateFormat
import java.time.Instant
import java.time.format.DateTimeFormatter
import java.util.{Date, TimeZone}
import scala.util.{Failure, Success, Try}

object TimeUtil {

  def now: Instant = Instant.now

  /** Returns the current timestamp in milliseconds */
  def currentEpochMs: Long = now.toEpochMilli

  /** Returns the current timestamp in seconds */
  def currentEpochSecond: Long = {
    now.getEpochSecond
  }

  def iso8601ToDate(str: String): Date = {
    val isoT = Try {
      val ta = DateTimeFormatter.ISO_INSTANT.parse(str)
      val instant = Instant.from(ta)
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

  def iso8601ToString(date: Date): String = {
    DateTimeFormatter.ISO_INSTANT.format(date.toInstant)
  }
}
