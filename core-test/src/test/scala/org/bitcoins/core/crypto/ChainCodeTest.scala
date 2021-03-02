package org.bitcoins.core.crypto

import org.bitcoins.testkitcore.gen.NumberGenerator
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

class ChainCodeTest extends BitcoinSUnitTest {
  behavior of "ChainCode"

  it must "not be constructable from invalid lengths byte vectors" in {
    forAll(NumberGenerator.bytevector.suchThat(_.length != 32)) { bytes =>
      intercept[IllegalArgumentException] {
        ChainCode.fromBytes(bytes)
      }
    }
  }
}
