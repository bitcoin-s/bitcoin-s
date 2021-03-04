package org.bitcoins.crypto

import org.bitcoins.testkitcore.gen.CryptoGenerators
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

/** Public key tests specific to the JVM */
class JvmECPublicKeyTest extends BitcoinSUnitTest {

  behavior of "JVMECPublicKeytest"

  it must "have serialization symmetry from ECPublicKey -> ECPoint -> ECPublicKey" in {
    CryptoContext.cryptoRuntime match {
      case _: BouncycastleCryptoRuntime | _: LibSecp256k1CryptoRuntime =>
        forAll(CryptoGenerators.publicKey) { pubKey =>
          val p = BouncyCastleUtil.decodePoint(pubKey)
          val pub2 = BouncyCastleUtil.decodePubKey(p, pubKey.isCompressed)
          assert(pubKey == pub2)
        }
      case _ => succeed
    }
  }
}
