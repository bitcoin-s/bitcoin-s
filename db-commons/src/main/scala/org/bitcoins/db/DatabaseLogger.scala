package org.bitcoins.db

import org.slf4j.Logger

/** Exposes access to the database interaction logger */
private[bitcoins] trait DatabaseLogger {
  private var _logger: Logger = _

  protected[bitcoins] def logger(implicit config: LoggerConfig): Logger = {
    if (_logger == null) {
      _logger = DatabaseLoggerImpl(config).getLogger
    }
    _logger
  }
}

private[db] case class DatabaseLoggerImpl(override val conf: LoggerConfig)
    extends AppLoggers {

  /**
    * @return the database interaction logger
    */
  def getLogger: Logger =
    getLoggerImpl(LoggerKind.Database)
}
