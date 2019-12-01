package org.bitcoins.wallet

import org.slf4j.Logger
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.db.AppLoggers

/** Exposes acccess to the wallet logger */
private[bitcoins] trait WalletLogger {
  private var _logger: Logger = _
  protected[bitcoins] def logger(implicit config: WalletAppConfig) = {
    if (_logger == null) {
      _logger = WalletLogger.getLogger
    }
    _logger
  }
}

private[bitcoins] object WalletLogger extends AppLoggers {

  /**
    * @return the generic wallet logger (i.e. everything not related to key handling)
    */
  def getLogger(implicit conf: WalletAppConfig): Logger =
    getLoggerImpl(LoggerKind.Wallet)
}
