package org.bitcoins.keymanager

import java.nio.file.Path

import org.bitcoins.core.crypto.AesPassword
import org.bitcoins.testkit.BitcoinSTestAppConfig

object KeyManagerTestUtil {

  def seedPath: Path = {
    BitcoinSTestAppConfig
      .tmpDir()
      .resolve(WalletStorage.ENCRYPTED_SEED_FILE_NAME)
  }

  val badPassphrase = AesPassword.fromString("bad-password").get
}
