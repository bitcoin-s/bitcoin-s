package org.bitcoins.wallet

import org.bitcoins.core.util.TimeUtil
import org.bitcoins.crypto.AesPassword
import org.bitcoins.keymanager.{DecryptedMnemonic, EncryptedMnemonicHelper}
import org.bitcoins.testkit.Implicits._
import org.bitcoins.testkit.core.gen.CryptoGenerators
import org.bitcoins.testkit.util.BitcoinSUnitTest

import scala.util.{Failure, Success}

class EncryptedMnemonicTest extends BitcoinSUnitTest {
  behavior of "EncryptedMnemonic"

  it must "fail to decrypt with a bad password" in {
    val password = AesPassword.fromNonEmptyString("good")
    val badPassword = AesPassword.fromNonEmptyString("bad")

    val mnemonicCode = CryptoGenerators.mnemonicCode.sampleSome
    val mnemonic = DecryptedMnemonic(mnemonicCode, TimeUtil.now)
    val encrypted = EncryptedMnemonicHelper.encrypt(mnemonic, password)

    val decrypted = encrypted.toMnemonic(badPassword)

    assert(decrypted.isFailure)

  }

  it must "have encryption/decryption symmetry" in {
    forAll(CryptoGenerators.mnemonicCode, CryptoGenerators.aesPassword) {
      (mnemonicCode, password) =>
        val mnemonic = DecryptedMnemonic(mnemonicCode, TimeUtil.now)
        val encrypted = EncryptedMnemonicHelper.encrypt(mnemonic, password)
        val decrypted = encrypted.toMnemonic(password) match {
          case Success(clear) => clear
          case Failure(exc)   => fail(exc)
        }
        assert(decrypted == mnemonicCode)
    }
  }
}
