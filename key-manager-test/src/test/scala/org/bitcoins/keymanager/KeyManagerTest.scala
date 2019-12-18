package org.bitcoins.keymanager

import org.bitcoins.core.crypto.{AesPassword, MnemonicCode}
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.util.BitcoinSUnitTest

class KeyManagerTest extends BitcoinSUnitTest {

  it must "initialize the key manager" in {
    val entropy = MnemonicCode.getEntropy256Bits
    val seedPath = BitcoinSTestAppConfig.tmpDir().resolve("encrypted-seed-bitcoin-s.json")
    val kmResult = KeyManager.initializeWithEntropy(entropy = entropy, seedPath = seedPath)
    val badPassphrase = AesPassword.fromString("bad-password").get
    val km = kmResult match {
      case InitializeKeyManagerSuccess(km) => km
      case err: InitializeKeyManagerError => fail(s"Failed to initialize key manager with err=${err}")
    }

    //verify we wrote the seed
    assert(WalletStorage.seedExists(seedPath), "KeyManager did not write the seed to disk!")

    val decryptedR: ReadMnemonicResult = WalletStorage.decryptMnemonicFromDisk(seedPath,badPassphrase)

    val mnemonic = decryptedR match {
      case ReadMnemonicSuccess(m) => m
      case err: ReadMnemonicError => fail(s"Failed to read mnemonic that was written by key manager with err=${err}")
    }

    assert(mnemonic.toEntropy == entropy, s"We did not read the same entropy that we wrote!")
  }
}
