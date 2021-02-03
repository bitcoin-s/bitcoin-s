package org.bitcoins.core.protocol.script

import org.bitcoins.testkit.core.gen.ScriptGenerators
import org.scalacheck.{Prop, Properties}

/** Created by chris on 6/22/16.
  */
class MultiSignatureScriptPubKeySpec
    extends Properties("MultiSignatureScriptPubKeySpec") {

  property("Serialization symmetry") =
    Prop.forAll(ScriptGenerators.multiSigScriptPubKey) {
      case (multiSigScriptPubKey, _) =>
        MultiSignatureScriptPubKey(
          multiSigScriptPubKey.hex) == multiSigScriptPubKey

    }
}
