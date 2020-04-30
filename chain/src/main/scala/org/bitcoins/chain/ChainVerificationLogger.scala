package org.bitcoins.chain

import org.bitcoins.db.{AppLoggers, LoggerConfig, MarkedLogger}

/** Exposes access to the chain verification logger */
private[bitcoins] trait ChainVerificationLogger {
  private var _logger: MarkedLogger = _
  protected[bitcoins] def logger(
      implicit config: LoggerConfig): MarkedLogger = {
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
  def getLogger: MarkedLogger =
    getLoggerImpl(LoggerKind.ChainVerification)
}
