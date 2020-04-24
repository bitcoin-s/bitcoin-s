package org.bitcoins.core.util

import java.time.{ZoneId, ZonedDateTime}

object TimeUtil {

  def currentEpochSecond: Long =
    ZonedDateTime.now(ZoneId.of("UTC")).toEpochSecond
}
