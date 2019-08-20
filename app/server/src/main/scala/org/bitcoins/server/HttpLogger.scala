package org.bitcoins.server

import org.slf4j.Logger
import org.bitcoins.db.AppConfig
import org.bitcoins.db.AppLoggers

/** Exposes access to the HTTP RPC server logger */
private[bitcoins] trait HttpLogger {
  private var _logger: Logger = _
  protected[bitcoins] def logger(implicit config: AppConfig) = {
    if (_logger == null) {
      _logger = HttpLogger.getLogger
    }
    _logger
  }
}

private[bitcoins] object HttpLogger extends AppLoggers {

  /**
    * @return the HTTP RPC server submobule logger
    */
  def getLogger(implicit conf: AppConfig): Logger =
    getLoggerImpl(LoggerKind.Http)
}
