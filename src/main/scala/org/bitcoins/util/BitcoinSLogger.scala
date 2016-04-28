package org.bitcoins.util

import org.slf4j.LoggerFactory

/**
 * Created by chris on 3/11/16.
 */
trait BitcoinSLogger {

  def logger = LoggerFactory.getLogger(this.getClass().toString)

}
