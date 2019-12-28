package org.bitcoins.keymanager

import java.nio.file.Path

import org.bitcoins.core.crypto.AesPassword
import org.bitcoins.keymanager.bip39.BIP39KeyManager
import org.bitcoins.testkit.BitcoinSTestAppConfig

object KeyManagerTestUtil {

  /** A temporary file that can be used as a seed path for testing */
  def tmpSeedPath: Path = {
    BitcoinSTestAppConfig
      .tmpDir()
      .resolve(WalletStorage.ENCRYPTED_SEED_FILE_NAME)
  }

  val badPassphrase: AesPassword = BIP39KeyManager.badPassphrase
}
