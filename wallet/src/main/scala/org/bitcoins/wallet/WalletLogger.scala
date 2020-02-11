package org.bitcoins.wallet

import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.db.{AppLoggers, MarkedLogger}

/** Exposes acccess to the wallet logger */
private[bitcoins] trait WalletLogger {
  private var _logger: MarkedLogger = _
  protected[bitcoins] def logger(implicit config: WalletAppConfig) = {
    if (_logger == null) {
      _logger = WalletLoggerImpl(config).getLogger
    }
    _logger
  }
}

private[wallet] case class WalletLoggerImpl(override val conf: WalletAppConfig)
    extends AppLoggers {

  /**
    * @return the generic wallet logger (i.e. everything not related to key handling)
    */
  def getLogger: MarkedLogger =
    getLoggerImpl(LoggerKind.Wallet)
}
