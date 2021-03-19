package org.bitcoins.core.protocol.script

import grizzled.slf4j.Logging
import org.bitcoins.testkitcore.gen.ScriptGenerators
import org.scalacheck.{Prop, Properties}

/** Created by chris on 6/22/16.
  */
class P2PKScriptSignatureSpec extends Properties("P2PKSpec") with Logging {

  property("Serialization symmetry") =
    Prop.forAll(ScriptGenerators.p2pkScriptSignature) { p2pkScriptSig =>
      P2PKScriptSignature(p2pkScriptSig.hex) == p2pkScriptSig
    }
}
