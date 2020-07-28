package org.bitcoins.wallet

import org.bitcoins.db.{AppLoggers, LoggerConfig}
import org.slf4j.Logger

/** Exposes acccess to the wallet logger */
private[bitcoins] trait WalletLogger {

  protected[bitcoins] def logger(implicit config: LoggerConfig): Logger = {
    WalletLoggerImpl(config).getLogger
  }
}

private[wallet] case class WalletLoggerImpl(override val conf: LoggerConfig)
    extends AppLoggers {

  /**
    * @return the generic wallet logger (i.e. everything not related to key handling)
    */
  def getLogger: Logger =
    getLoggerImpl(LoggerKind.Wallet)
}
