package org.bitcoins.node

import org.slf4j.Logger
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.db.AppLoggers

/** Exposes access to the P2P submodule logger */
private[bitcoins] trait P2PLogger {
  private var _logger: Logger = _
  protected def logger(implicit config: NodeAppConfig) = {
    if (_logger == null) {
      _logger = P2PLogger.getLogger
    }
    _logger
  }
}

private[bitcoins] object P2PLogger extends AppLoggers {

  /**
    * @return the peer-to-peer submobule logger
    */
  def getLogger(implicit conf: NodeAppConfig) = getLoggerImpl(LoggerKind.P2P)
}
