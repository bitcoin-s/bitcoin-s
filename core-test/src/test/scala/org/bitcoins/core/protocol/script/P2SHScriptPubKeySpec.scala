package org.bitcoins.core.protocol.script

import org.bitcoins.testkitcore.gen.ScriptGenerators
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

/** Created by chris on 6/24/16.
  */
class P2SHScriptPubKeySpec extends BitcoinSUnitTest {

  it must "symmetrical serialization" in {
    forAll(ScriptGenerators.p2shScriptPubKey) { case (p2shScriptPubKey, _, _) =>
      assert(P2SHScriptPubKey(p2shScriptPubKey.hex) == p2shScriptPubKey)
    }
  }
}
