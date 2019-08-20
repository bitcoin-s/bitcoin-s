package org.bitcoins.db

import org.slf4j.Logger

/** Exposes access to the database interaction logger */
private[bitcoins] trait DatabaseLogger {
  private var _logger: Logger = _
  protected[bitcoins] def logger(implicit config: AppConfig) = {
    if (_logger == null) {
      _logger = DatabaseLogger.getLogger
    }
    _logger
  }
}

private[bitcoins] object DatabaseLogger extends AppLoggers {

  /**
    * @return the database interaction logger
    */
  def getLogger(implicit conf: AppConfig): Logger =
    getLoggerImpl(LoggerKind.Database)
}
