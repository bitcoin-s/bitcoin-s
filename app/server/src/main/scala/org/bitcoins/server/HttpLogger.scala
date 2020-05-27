package org.bitcoins.server

import org.bitcoins.db.{AppLoggers, LoggerConfig}
import org.slf4j.Logger

/** Exposes access to the HTTP RPC server logger */
private[bitcoins] trait HttpLogger {
  private var _logger: Logger = _
  protected[bitcoins] def logger(implicit config: LoggerConfig): Logger = {
    if (_logger == null) {
      _logger = HttpLoggerImpl(config).getLogger
    }
    _logger
  }
}

private[server] case class HttpLoggerImpl(override val conf: LoggerConfig)
    extends AppLoggers {

  /**
    * @return the HTTP RPC server submobule logger
    */
  def getLogger: Logger =
    getLoggerImpl(LoggerKind.Http)
}
