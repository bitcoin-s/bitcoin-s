package org.bitcoins.keymanager

import org.bitcoins.db.{AppLoggers, LoggerConfig}
import org.slf4j.Logger

/** Exposes access to the key manager logger */
private[bitcoins] trait KeyManagerLogger {
  private var _logger: Logger = _
  protected[bitcoins] def logger(implicit config: LoggerConfig): Logger = {
    if (_logger == null) {
      _logger = KeyManagerLoggerImpl(config).getLogger
    }
    _logger
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
