package org.bitcoins.chain

import org.bitcoins.db.AppLoggers
import org.bitcoins.chain.config.ChainAppConfig
import org.slf4j.Logger

/** Exposes access to the chain verification logger */
private[bitcoins] trait ChainVerificationLogger {
  private var _logger: Logger = _
  protected[bitcoins] def logger(implicit config: ChainAppConfig) = {
    if (_logger == null) {
      _logger = ChainVerificationLogger.getLogger
    }
    _logger
  }
}

private[bitcoins] object ChainVerificationLogger extends AppLoggers {

  /**
    * @return the chain verification submobule logger
    */
  def getLogger(implicit conf: ChainAppConfig): Logger =
    getLoggerImpl(LoggerKind.ChainVerification)
}
