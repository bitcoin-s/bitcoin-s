package org.bitcoins.core.protocol.script

import org.bitcoins.core.gen.ScriptGenerators
import org.scalacheck.{Prop, Properties}

/**
  * Created by chris on 6/22/16.
  */
class P2PKHScriptPubKeySpec extends Properties("P2PKHScriptPubKeySpec") {

  property("Serialization symmetry") =
    Prop.forAll(ScriptGenerators.p2pkhScriptPubKey) { case (p2pkhScriptPubKey, _) =>
      P2PKHScriptPubKey(p2pkhScriptPubKey.hex) == p2pkhScriptPubKey

    }
}
