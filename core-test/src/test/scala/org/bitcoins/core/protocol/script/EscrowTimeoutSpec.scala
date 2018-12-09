package org.bitcoins.core.protocol.script

import org.bitcoins.core.gen.ScriptGenerators
import org.scalacheck.{Prop, Properties}

/**
  * Created by chris on 3/27/17.
  */
class EscrowTimeoutSpec extends Properties("CSVEscrowWithTimeoutSpec") {

  property("serialization symmetry") = {
    Prop.forAll(ScriptGenerators.escrowTimeoutScriptPubKey) {
      case (escrowTimeout, _) =>
        EscrowTimeoutScriptPubKey(escrowTimeout.hex) == escrowTimeout &&
          EscrowTimeoutScriptPubKey(escrowTimeout.escrow, escrowTimeout.timeout) == escrowTimeout &&
          EscrowTimeoutScriptPubKey.isValidEscrowTimeout(escrowTimeout.asm)
    }
  }
}
