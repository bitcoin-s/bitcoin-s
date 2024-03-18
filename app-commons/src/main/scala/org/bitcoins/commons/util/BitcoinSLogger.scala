package org.bitcoins.commons.util

import org.slf4j.{Logger, LoggerFactory}

trait BitcoinSLogger {

  def logger: Logger = LoggerFactory.getLogger(getClass)
}
