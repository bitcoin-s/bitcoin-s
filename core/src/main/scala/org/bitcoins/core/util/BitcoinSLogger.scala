package org.bitcoins.core.util

import org.slf4j.LoggerFactory
import org.slf4j.Logger

/**
 * Created by chris on 3/11/16.
 */
abstract class BitcoinSLogger {

  def logger: Logger = LoggerFactory.getLogger(this.getClass().toString)

}

object BitcoinSLogger extends BitcoinSLogger
