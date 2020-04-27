package org.bitcoins.keymanager.bip39

import org.bitcoins.crypto.AesPassword
import org.bitcoins.keymanager.KeyManagerUnlockError
import org.bitcoins.testkit.keymanager.{KeyManagerTestUtil, KeyManagerUnitTest}

class BIP39LockedKeyManagerTest extends KeyManagerUnitTest {

  it must "be able to read a locked mnemonic from disk" in {
    val bip39PwOpt = KeyManagerTestUtil.bip39PasswordOpt
    val km = withInitializedKeyManager(bip39PasswordOpt = bip39PwOpt)

    val unlockedE =
      BIP39LockedKeyManager.unlock(KeyManagerTestUtil.badPassphrase,
                                   bip39PasswordOpt = bip39PwOpt,
                                   km.kmParams)

    val unlockedKm = unlockedE match {
      case Right(km) => km
      case Left(err) => fail(s"Failed to unlock key manager ${err}")
    }

    assert(km == unlockedKm,
           s"Unlocked key manager must be the same was the pre-locked one")
  }

  it must "fail to read bad json in the seed file" in {
    val km = withInitializedKeyManager()
    val badPassword = AesPassword.fromString("other bad password").get
    val unlockedE = BIP39LockedKeyManager.unlock(passphrase = badPassword,
                                                 bip39PasswordOpt = None,
                                                 kmParams = km.kmParams)

    unlockedE match {
      case Left(KeyManagerUnlockError.BadPassword) => succeed
      case result @ (Left(_) | Right(_)) =>
        fail(
          s"Expected to fail test with ${KeyManagerUnlockError.BadPassword} got ${result}")
    }
  }

  it must "fail if the seedPath is not found" in {
    val badSeedPath = KeyManagerTestUtil.tmpSeedPath
    val km = withInitializedKeyManager()

    val badPath = km.kmParams.copy(seedPath = badSeedPath)
    val badPassword = AesPassword.fromString("other bad password").get
    val unlockedE = BIP39LockedKeyManager.unlock(badPassword, None, badPath)

    unlockedE match {
      case Left(KeyManagerUnlockError.MnemonicNotFound) => succeed
      case result @ (Left(_) | Right(_)) =>
        fail(
          s"Expected to fail test with ${KeyManagerUnlockError.MnemonicNotFound} got ${result}")
    }
  }
}
