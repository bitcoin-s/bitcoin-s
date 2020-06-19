package org.bitcoins.testkit.keymanager

import org.bitcoins.core.crypto.MnemonicCode
import org.bitcoins.keymanager.KeyManagerParams
import org.bitcoins.keymanager.bip39.BIP39KeyManager
import org.bitcoins.testkit.util.BitcoinSUnitTest
import scodec.bits.BitVector

trait KeyManagerUnitTest extends BitcoinSUnitTest {

  def withInitializedKeyManager(
      kmParams: KeyManagerParams = KeyManagerTestUtil.createKeyManagerParams(),
      entropy: BitVector = MnemonicCode.getEntropy256Bits,
      bip39PasswordOpt: Option[String] =
        KeyManagerTestUtil.bip39PasswordOpt): BIP39KeyManager = {
    val kmResult = BIP39KeyManager.initializeWithEntropy(
      entropy = entropy,
      bip39PasswordOpt = bip39PasswordOpt,
      kmParams = kmParams
    )

    val km = kmResult match {
      case Right(km) => km
      case Left(err) =>
        fail(s"Failed to initialize key manager with err=${err}")
    }

    km
  }
}
