package org.bitcoins.core.protocol.script

import org.bitcoins.testkit.core.gen.ScriptGenerators
import org.scalacheck.{Prop, Properties}

/**
  * Created by chris on 6/22/16.
  */
class P2PKHScriptSignatureSpec extends Properties("P2PKHSpec") {

  property("Serialization symmetry") =
    Prop.forAll(ScriptGenerators.p2pkhScriptSignature) { p2pkhScriptSig =>
      P2PKHScriptSignature(p2pkhScriptSig.hex) == p2pkhScriptSig

    }
}
