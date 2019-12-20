package org.bitcoins.keymanager

import org.bitcoins.core.crypto.AesPassword

class LockedKeyManagerTest extends KeyManagerUnitTest {

  it must "be able to read a locked mnemonic from disk" in {
    val km = withInitializedKeyManager()

    val unlockedKm = LockedKeyManager.unlock(KeyManagerTestUtil.badPassphrase, km.kmParams) match {
      case UnlockKeyManagerSuccess(km) => km
      case err: UnlockKeyManagerError => fail(s"Failed to unlock key manager ${err}")
    }

    assert(km == unlockedKm, s"Unlocked key manager must be the same was the pre-locked one")
  }


  it must "fail to read bad json in the seed file" in {
    val km = withInitializedKeyManager()
    val badPassword = AesPassword.fromString("other bad password").get
    LockedKeyManager.unlock(passphrase = badPassword, kmParams = km.kmParams) match {
      case UnlockKeyManagerError.BadPassword => succeed
      case result: UnlockKeyManagerResult =>
        fail(s"Expected to fail test with ${UnlockKeyManagerError.BadPassword} got ${result}")
    }
  }


  it must "fail if the seedPath is not found" in {
    val badSeedPath = KeyManagerTestUtil.seedPath
    val km = withInitializedKeyManager()

    val badPath = km.kmParams.copy(seedPath = badSeedPath)
    val badPassword = AesPassword.fromString("other bad password").get
    LockedKeyManager.unlock(badPassword, badPath) match {
      case UnlockKeyManagerError.MnemonicNotFound => succeed
      case result: UnlockKeyManagerResult =>
        fail(s"Expected to fail test with ${UnlockKeyManagerError.MnemonicNotFound} got ${result}")
    }
  }
}
