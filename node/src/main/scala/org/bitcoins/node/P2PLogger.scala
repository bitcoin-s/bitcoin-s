package org.bitcoins.node

import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.db.{AppLoggers, MarkedLogger}

/** Exposes access to the P2P submodule logger */
private[bitcoins] trait P2PLogger {
  private var _logger: MarkedLogger = _
  protected def logger(implicit config: NodeAppConfig) = {
    if (_logger == null) {
      _logger = P2PLoggerImpl(config).getLogger
    }
    _logger
  }
}

private[node] case class P2PLoggerImpl(override val conf: NodeAppConfig)
    extends AppLoggers {

  /**
    * @return the peer-to-peer submobule logger
    */
  def getLogger: MarkedLogger =
    getLoggerImpl(LoggerKind.P2P)
}
