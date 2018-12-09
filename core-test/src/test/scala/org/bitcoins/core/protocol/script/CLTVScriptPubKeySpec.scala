package org.bitcoins.core.protocol.script

import org.bitcoins.core.gen.ScriptGenerators
import org.scalacheck.{Prop, Properties}

/**
  * Created by tom on 8/23/16.
  */
class CLTVScriptPubKeySpec extends Properties("CLTVScriptPubKeySpec") {
  property("Serialization symmetry") =
    Prop.forAll(ScriptGenerators.cltvScriptPubKey) {
      case (cltvScriptPubKey, _) =>
        CLTVScriptPubKey(cltvScriptPubKey.hex) == cltvScriptPubKey
    }
}
