package org.bitcoins.core.protocol.script.descriptor

import org.bitcoins.testkitcore.util.BitcoinSUnitTest

class KeyExpressionTest extends BitcoinSUnitTest {

  behavior of "KeyExpression"

  it must "serialize and deserialize key origin examples in BIP380" in {
    val str0 = "[deadbeef/0'/0'/0']"
    val keyOrigin = KeyOriginExpression.fromString(str0)
    assert(str0 == keyOrigin.toString)
  }
}
