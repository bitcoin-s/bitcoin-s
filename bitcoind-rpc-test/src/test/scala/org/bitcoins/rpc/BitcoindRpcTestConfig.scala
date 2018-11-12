package org.bitcoins.rpc

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.util.Properties

object BitcoindRpcTestConfig {

  val DEFAULT_TIMEOUT: FiniteDuration =
    if (Properties.isMac) 5.minutes else 1.minute
}
