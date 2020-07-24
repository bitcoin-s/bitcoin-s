package org.bitcoins.keymanager

import org.bitcoins.db.{AppLoggers, LoggerConfig}
import org.slf4j.Logger

/** Exposes access to the key manager logger */
private[bitcoins] trait KeyManagerLogger {

  protected[bitcoins] def logger(implicit config: LoggerConfig): Logger = {
    KeyManagerLoggerImpl(config).getLogger
  }
}

private[keymanager] case class KeyManagerLoggerImpl(
    override val conf: LoggerConfig)
    extends AppLoggers {

  /**
    * @return the generic Key Manager logger (i.e. everything related to secret keys)
    */
  def getLogger: Logger =
    getLoggerImpl(LoggerKind.KeyHandling)
}
