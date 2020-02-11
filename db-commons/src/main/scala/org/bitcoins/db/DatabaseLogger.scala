package org.bitcoins.db

/** Exposes access to the database interaction logger */
private[bitcoins] trait DatabaseLogger {
  private var _logger: MarkedLogger = _
  protected[bitcoins] def logger(implicit config: AppConfig) = {
    if (_logger == null) {
      _logger = DatabaseLoggerImpl(config).getLogger
    }
    _logger
  }
}

private[db] case class DatabaseLoggerImpl(override val conf: AppConfig)
    extends AppLoggers {

  /**
    * @return the database interaction logger
    */
  def getLogger: MarkedLogger =
    getLoggerImpl(LoggerKind.Database)
}
