package org.bitcoins.chain

import org.bitcoins.db.{AppLoggers, MarkedLogger}
import org.bitcoins.chain.config.ChainAppConfig

/** Exposes access to the chain verification logger */
private[bitcoins] trait ChainVerificationLogger {
  private var _logger: MarkedLogger = _
  protected[bitcoins] def logger(implicit config: ChainAppConfig) = {
    if (_logger == null) {
      _logger = ChainVerificationLoggerImpl(config).getLogger
    }
    _logger
  }
}

private[chain] case class ChainVerificationLoggerImpl(
    override val conf: ChainAppConfig)
    extends AppLoggers {

  /**
    * @return the chain verification submobule logger
    */
  def getLogger: MarkedLogger =
    getLoggerImpl(LoggerKind.ChainVerification)
}
