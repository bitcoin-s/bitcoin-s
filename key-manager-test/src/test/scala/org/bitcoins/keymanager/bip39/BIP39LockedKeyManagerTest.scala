package org.bitcoins.keymanager.bip39

import org.bitcoins.core.crypto.AesPassword
import org.bitcoins.keymanager.{KeyManagerTestUtil, KeyManagerUnitTest, KeyManagerUnlockError}

class BIP39LockedKeyManagerTest extends KeyManagerUnitTest {

  it must "be able to read a locked mnemonic from disk" in {
    val km = withInitializedKeyManager()

    val unlockedKm = BIP39LockedKeyManager.unlock(KeyManagerTestUtil.badPassphrase, km.kmParams) match {
      case Right(km) => km
      case Left(err) => fail(s"Failed to unlock key manager ${err}")
    }

    assert(km == unlockedKm, s"Unlocked key manager must be the same was the pre-locked one")
  }


  it must "fail to read bad json in the seed file" in {
    val km = withInitializedKeyManager()
    val badPassword = AesPassword.fromString("other bad password").get
    BIP39LockedKeyManager.unlock(passphrase = badPassword, kmParams = km.kmParams) match {
      case Left(KeyManagerUnlockError.BadPassword) => succeed
      case result @ (Left(_) | Right(_)) =>
        fail(s"Expected to fail test with ${KeyManagerUnlockError.BadPassword} got ${result}")
    }
  }


  it must "fail if the seedPath is not found" in {
    val badSeedPath = KeyManagerTestUtil.tmpSeedPath
    val km = withInitializedKeyManager()

    val badPath = km.kmParams.copy(seedPath = badSeedPath)
    val badPassword = AesPassword.fromString("other bad password").get
    BIP39LockedKeyManager.unlock(badPassword, badPath) match {
      case Left(KeyManagerUnlockError.MnemonicNotFound) => succeed
      case result @ (Left(_) | Right(_)) =>
        fail(s"Expected to fail test with ${KeyManagerUnlockError.MnemonicNotFound} got ${result}")
    }
  }
}
