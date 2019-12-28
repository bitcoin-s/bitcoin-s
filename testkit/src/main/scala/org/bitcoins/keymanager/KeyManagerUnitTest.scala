package org.bitcoins.keymanager

import org.bitcoins.core.config.Networks
import org.bitcoins.core.crypto.MnemonicCode
import org.bitcoins.core.hd.HDPurposes
import org.bitcoins.keymanager.bip39.BIP39KeyManager
import org.bitcoins.testkit.util.BitcoinSUnitTest
import org.scalacheck.Gen
import scodec.bits.BitVector

trait KeyManagerUnitTest extends BitcoinSUnitTest {

  def createKeyManagerParams(): KeyManagerParams = {
    val seedPath = KeyManagerTestUtil.tmpSeedPath
    KeyManagerParams(seedPath = seedPath,
                     purpose = Gen.oneOf(HDPurposes.all).sample.get,
                     network = Gen.oneOf(Networks.knownNetworks).sample.get)
  }

  def withInitializedKeyManager(
      kmParams: KeyManagerParams = createKeyManagerParams(),
      entropy: BitVector = MnemonicCode.getEntropy256Bits): BIP39KeyManager = {
    val kmResult = BIP39KeyManager.initializeWithEntropy(
      entropy = entropy,
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
