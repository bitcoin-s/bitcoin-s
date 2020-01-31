package org.bitcoins.chain

import org.bitcoins.db.{AppLoggers, MarkedLogger}
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.core.config.NetworkParameters

/** Exposes access to the chain verification logger */
private[bitcoins] trait ChainVerificationLogger {
  private var _logger: MarkedLogger = _
  protected[bitcoins] def logger(implicit config: ChainAppConfig) = {
    if (_logger == null) {
      _logger = ChainVerificationLoggerImpl(config.network).getLogger
    }
    _logger
  }
}

private[chain] case class ChainVerificationLoggerImpl(
    network: NetworkParameters)
    extends AppLoggers {

  /**
    * @return the chain verification submobule logger
    */
  def getLogger(implicit conf: ChainAppConfig): MarkedLogger =
    getLoggerImpl(LoggerKind.ChainVerification)
}
