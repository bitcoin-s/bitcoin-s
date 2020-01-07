package org.bitcoins.keymanager

import java.nio.file.Path

import org.bitcoins.core.config.Networks
import org.bitcoins.core.crypto.AesPassword
import org.bitcoins.core.hd.HDPurposes
import org.bitcoins.keymanager.bip39.BIP39KeyManager
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.core.gen.CryptoGenerators
import org.scalacheck.Gen

object KeyManagerTestUtil {

  /** A temporary file that can be used as a seed path for testing */
  def tmpSeedPath: Path = {
    BitcoinSTestAppConfig
      .tmpDir()
      .resolve(WalletStorage.ENCRYPTED_SEED_FILE_NAME)
  }

  def createKeyManagerParams(): KeyManagerParams = {
    val seedPath = KeyManagerTestUtil.tmpSeedPath
    KeyManagerParams(seedPath = seedPath,
                     purpose = Gen.oneOf(HDPurposes.all).sample.get,
                     network = Gen.oneOf(Networks.knownNetworks).sample.get)
  }

  def bip39PasswordOpt: Option[String] = {
    if (scala.util.Random.nextBoolean()) {
      Some(bip39Password)
    } else {
      None
    }
  }

  def bip39Password: String = {
    CryptoGenerators.bip39Password.sample.get
  }

  val badPassphrase: AesPassword = BIP39KeyManager.badPassphrase
}
