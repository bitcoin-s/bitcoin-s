package org.bitcoins.core.protocol.script

import org.bitcoins.testkit.core.gen.ScriptGenerators
import org.bitcoins.core.util.BitcoinSLogger
import org.scalacheck.{Prop, Properties}

/**
  * Created by chris on 6/22/16.
  */
class P2PKScriptSignatureSpec
    extends Properties("P2PKSpec")
    with BitcoinSLogger {

  property("Serialization symmetry") =
    Prop.forAll(ScriptGenerators.p2pkScriptSignature) { p2pkScriptSig =>
      P2PKScriptSignature(p2pkScriptSig.hex) == p2pkScriptSig
    }
}
