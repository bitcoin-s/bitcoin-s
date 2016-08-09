package org.bitcoins.core.protocol.script

import org.bitcoins.core.gen.ScriptGenerators
import org.scalacheck.{Properties, Prop}

/**
  * Created by tom on 8/23/16.
  */
class CLTVScriptPubKeySpec extends Properties("CLTVScriptPubKeySpec") {
  property("Serialization symmetry") =
    Prop.forAll(ScriptGenerators.cltvScriptPubKey) { cltvScriptPubKey =>
      CLTVScriptPubKey(cltvScriptPubKey.hex) == cltvScriptPubKey
    }
}
