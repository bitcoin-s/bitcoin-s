package org.bitcoins.benchmark
import org.bitcoins.core.util.BitcoinSLogger

import scala.concurrent.duration.{FiniteDuration, DurationLong}

object BenchUtil extends BitcoinSLogger {

  /**
    * Current time stamp converted to a finite duration
    */
  def now: FiniteDuration = System.currentTimeMillis.millis

  /**
    * Times the given operation
    *
    * @param printTime Should we print how long time the
    *                  operation took
    */
  def timeOperation(
      operation: () => Any,
      printTime: Boolean = false): FiniteDuration = {
    val t0 = now
    val _ = operation()
    val time = now - t0
    if (printTime) {
      logger.info(s"elapsed time: $time")
    }
    time
  }
}
