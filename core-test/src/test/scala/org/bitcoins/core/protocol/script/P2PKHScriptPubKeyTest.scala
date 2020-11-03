package org.bitcoins.core.protocol.script

import org.bitcoins.testkit.Implicits._
import org.bitcoins.testkit.core.gen.CryptoGenerators
import org.bitcoins.testkit.util.BitcoinSUnitTest

class P2PKHScriptPubKeyTest extends BitcoinSUnitTest {

  "P2PKHScriptPubKey" must "return the pubkeyhash" in {
    val hash = CryptoGenerators.sha256Hash160Digest.sampleSome
    val p2pkhScriptPubKey = P2PKHScriptPubKey(hash)
    p2pkhScriptPubKey.pubKeyHash must be(hash)
  }
}
