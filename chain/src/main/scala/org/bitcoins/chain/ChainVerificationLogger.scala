package org.bitcoins.chain

import org.bitcoins.db.{AppLoggers, LoggerConfig}
import org.slf4j.Logger

/** Exposes access to the chain verification logger */
private[bitcoins] trait ChainVerificationLogger {
  private var _logger: Logger = _
  protected[bitcoins] def logger(implicit config: LoggerConfig): Logger = {
    if (_logger == null) {
      _logger = ChainVerificationLoggerImpl(config).getLogger
    }
    _logger
  }
}

private[chain] case class ChainVerificationLoggerImpl(
    override val conf: LoggerConfig)
    extends AppLoggers {

  /**
    * @return the chain verification submobule logger
    */
  def getLogger: Logger =
    getLoggerImpl(LoggerKind.ChainVerification)
}
