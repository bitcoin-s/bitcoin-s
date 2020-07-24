package org.bitcoins.server

import org.bitcoins.db.{AppLoggers, LoggerConfig}
import org.slf4j.Logger

/** Exposes access to the HTTP RPC server logger */
private[bitcoins] trait HttpLogger {

  protected[bitcoins] def logger(implicit config: LoggerConfig): Logger = {
    HttpLoggerImpl(config).getLogger
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
