package org.bitcoins.server

import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.db.{AppConfig, AppLoggers, MarkedLogger}

/** Exposes access to the HTTP RPC server logger */
private[bitcoins] trait HttpLogger {
  private var _logger: MarkedLogger = _
  protected[bitcoins] def logger(implicit config: AppConfig) = {
    if (_logger == null) {
      _logger = HttpLoggerImpl(config.network).getLogger
    }
    _logger
  }
}

private[server] case class HttpLoggerImpl(network: NetworkParameters)
    extends AppLoggers {

  /**
    * @return the HTTP RPC server submobule logger
    */
  def getLogger(implicit conf: AppConfig): MarkedLogger =
    getLoggerImpl(LoggerKind.Http)
}
