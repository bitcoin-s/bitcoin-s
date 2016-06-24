package org.bitcoins.core.protocol.script

import org.scalacheck.{Prop, Properties}

/**
  * Created by chris on 6/22/16.
  */
class P2PKScriptPubKeySpec extends Properties("P2PKScriptPubKeySpec") {

  property("Serialization symmetry") =
    Prop.forAll(ScriptGenerators.p2pkScriptPubKey) { p2pkScriptPubKey =>
      P2PKScriptPubKey(p2pkScriptPubKey.hex) == p2pkScriptPubKey

    }
}
