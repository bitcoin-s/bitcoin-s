package org.bitcoins.wallet

import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.db.{AppLoggers, MarkedLogger}

/** Exposes acccess to the wallet logger */
private[bitcoins] trait WalletLogger {
  private var _logger: MarkedLogger = _
  protected[bitcoins] def logger(implicit config: WalletAppConfig) = {
    if (_logger == null) {
      _logger = WalletLoggerImpl(config.network).getLogger
    }
    _logger
  }
}

private[wallet] case class WalletLoggerImpl(network: NetworkParameters)
    extends AppLoggers {

  /**
    * @return the generic wallet logger (i.e. everything not related to key handling)
    */
  def getLogger(implicit conf: WalletAppConfig): MarkedLogger =
    getLoggerImpl(LoggerKind.Wallet)
}
