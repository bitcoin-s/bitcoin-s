package org.bitcoins.db

import org.bitcoins.keymanager.config.KeyManagerAppConfig

object PostgresUtil {

  def getSchemaName(moduleName: String, walletName: String): String = {
    if (walletName == KeyManagerAppConfig.DEFAULT_WALLET_NAME) {
      moduleName
    } else {
      s"${moduleName}_$walletName"
    }
  }
}
