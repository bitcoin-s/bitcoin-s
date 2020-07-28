package org.bitcoins.node

import org.bitcoins.db.{AppLoggers, LoggerConfig}
import org.bitcoins.node.config.NodeAppConfig
import org.slf4j.Logger

/** Exposes access to the P2P submodule logger */
private[bitcoins] trait P2PLogger {

  protected def logger(implicit config: NodeAppConfig): Logger = {
    P2PLoggerImpl(config).getLogger
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
