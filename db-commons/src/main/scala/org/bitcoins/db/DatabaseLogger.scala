package org.bitcoins.db

import org.bitcoins.core.config.NetworkParameters

/** Exposes access to the database interaction logger */
private[bitcoins] trait DatabaseLogger {
  private var _logger: MarkedLogger = _
  protected[bitcoins] def logger(implicit config: AppConfig) = {
    if (_logger == null) {
      _logger = DatabaseLoggerImpl(config.network).getLogger
    }
    _logger
  }
}

private[db] case class DatabaseLoggerImpl(network: NetworkParameters)
    extends AppLoggers {

  /**
    * @return the database interaction logger
    */
  def getLogger(implicit conf: AppConfig): MarkedLogger =
    getLoggerImpl(LoggerKind.Database)
}
