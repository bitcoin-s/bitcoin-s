package org.bitcoins.server

import org.bitcoins.db.{AppConfig, AppLoggers, MarkedLogger}

/** Exposes access to the HTTP RPC server logger */
private[bitcoins] trait HttpLogger {
  private var _logger: MarkedLogger = _
  protected[bitcoins] def logger(implicit config: AppConfig) = {
    if (_logger == null) {
      _logger = HttpLoggerImpl(config).getLogger
    }
    _logger
  }
}

private[server] case class HttpLoggerImpl(override val conf: AppConfig)
    extends AppLoggers {

  /**
    * @return the HTTP RPC server submobule logger
    */
  def getLogger: MarkedLogger =
    getLoggerImpl(LoggerKind.Http)
}
