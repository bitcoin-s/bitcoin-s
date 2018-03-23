package org.bitcoins.core.protocol.script

import org.bitcoins.core.gen.ScriptGenerators
import org.bitcoins.core.util.BitcoinSLogger
import org.scalacheck.{ Prop, Properties }

/**
 * Created by chris on 3/28/17.
 */
class EscrowTimeoutScriptSigSpec extends Properties("EscrowWithTimeoutScriptSigSpec") {

  property("serialization symmetry") =
    Prop.forAll(ScriptGenerators.escrowTimeoutScriptSig) { scriptSig =>
      EscrowTimeoutScriptSignature(scriptSig.hex) == scriptSig &&
        EscrowTimeoutScriptSignature.fromAsm(scriptSig.asm) == scriptSig
    }
}
