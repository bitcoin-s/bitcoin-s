package org.bitcoins.wallet

import org.bitcoins.core.crypto.{AesPassword, MnemonicCode}
import org.bitcoins.testkit.core.gen.CryptoGenerators
import org.bitcoins.testkit.util.BitcoinSUnitTest
import org.bitcoins.testkit.Implicits._

import scala.util.{Failure, Success}

class EncryptedMnemonicTest extends BitcoinSUnitTest {
  behavior of "EncryptedMnemonic"

  it must "fail to decrypt with a bad password" in {
    val password = AesPassword.fromNonEmptyString("good")
    val badPassword = AesPassword.fromNonEmptyString("bad")

    val mnemonic = CryptoGenerators.mnemonicCode.sampleSome
    val encrypted = EncryptedMnemonicHelper.encrypt(mnemonic, password)

    val decrypted = encrypted.toMnemonic(badPassword)

    assert(decrypted.isFailure)

  }

  it must "have encryption/decryption symmetry" in {
    forAll(CryptoGenerators.mnemonicCode, CryptoGenerators.aesPassword) {
      (code, password) =>
        val encrypted = EncryptedMnemonicHelper.encrypt(code, password)
        val decrypted = encrypted.toMnemonic(password) match {
          case Success(clear) => clear
          case Failure(exc)   => fail(exc)
        }
        assert(decrypted == code)
    }
  }
}
