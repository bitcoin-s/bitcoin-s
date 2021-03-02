package org.bitcoins.testkit.keymanager

import org.bitcoins.core.crypto.MnemonicCode
import org.bitcoins.core.wallet.keymanagement.KeyManagerParams
import org.bitcoins.crypto.AesPassword
import org.bitcoins.keymanager.bip39.BIP39KeyManager
import org.bitcoins.testkitcore.util.BitcoinSUnitTest
import scodec.bits.BitVector

trait KeyManagerApiUnitTest extends BitcoinSUnitTest {

  def withInitializedKeyManager(
      aesPasswordOpt: Option[AesPassword] = KeyManagerTestUtil.aesPasswordOpt,
      kmParams: KeyManagerParams = KeyManagerTestUtil.createKeyManagerParams(),
      entropy: BitVector = MnemonicCode.getEntropy256Bits,
      bip39PasswordOpt: Option[String] =
        KeyManagerTestUtil.bip39PasswordOpt): BIP39KeyManager = {
    val kmResult = BIP39KeyManager.initializeWithEntropy(
      aesPasswordOpt = aesPasswordOpt,
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
