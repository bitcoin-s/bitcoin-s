package org.bitcoins.core.protocol.script

import org.bitcoins.testkitcore.Implicits._
import org.bitcoins.testkitcore.gen.{CryptoGenerators, ScriptGenerators}
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

class P2PKHScriptPubKeyTest extends BitcoinSUnitTest {

  "P2PKHScriptPubKey" must "return the pubkeyhash" in {
    val hash = CryptoGenerators.sha256Hash160Digest.sampleSome
    val p2pkhScriptPubKey = P2PKHScriptPubKey(hash)
    p2pkhScriptPubKey.pubKeyHash must be(hash)
  }

  it must "serialization symmetry" in {
    forAll(ScriptGenerators.p2pkhScriptPubKey) { case (p2pkhScriptPubKey, _) =>
      assert(P2PKHScriptPubKey(p2pkhScriptPubKey.hex) == p2pkhScriptPubKey)
    }
  }

  it must "find pubkeyhash in scriptPubKey" in {
    forAll(CryptoGenerators.sha256Hash160Digest) { hash =>
      val scriptPubKey = P2PKHScriptPubKey(hash)
      assert(scriptPubKey.pubKeyHash == hash)
    }
  }
}
