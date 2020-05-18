package org.bitcoins.node

import org.bitcoins.db.{AppLoggers, LoggerConfig}
import org.slf4j.Logger

/** Exposes access to the P2P submodule logger */
private[bitcoins] trait P2PLogger {
  private var _logger: Logger = _
  protected def logger(implicit config: LoggerConfig): Logger = {
    if (_logger == null) {
      _logger = P2PLoggerImpl(config).getLogger
    }
    _logger
  }
}

private[node] case class P2PLoggerImpl(override val conf: LoggerConfig)
    extends AppLoggers {

  /**
    * @return the peer-to-peer submobule logger
    */
  def getLogger: Logger =
    getLoggerImpl(LoggerKind.P2P)
}
