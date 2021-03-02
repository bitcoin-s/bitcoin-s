package org.bitcoins.core.protocol.script

import org.bitcoins.testkitcore.gen.ScriptGenerators
import org.scalacheck.{Prop, Properties}

/** Created by chris on 6/22/16.
  */
class MultiSignatureScriptSignatureSpec
    extends Properties("MultiSignatureScriptSigSpec") {

  property("Serialization symmetry") =
    Prop.forAll(ScriptGenerators.multiSignatureScriptSignature) {
      multiSigScriptSig =>
        MultiSignatureScriptSignature(
          multiSigScriptSig.hex) == multiSigScriptSig

    }
}
