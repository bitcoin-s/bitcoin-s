package org.bitcoins.core.protocol.script

import org.bitcoins.core.gen.ScriptGenerators
import org.scalacheck.{Prop, Properties}

/**
  * Created by chris on 3/27/17.
  */
class CSVEscrowTimeoutSpec extends Properties("CSVEscrowWithTimeoutSpec") {

  property("serialization symmetry") = {
    Prop.forAll(ScriptGenerators.csvEscrowTimeoutScriptPubKey) { case (csvEscrowTimeout,_) =>
      CSVEscrowTimeoutScriptPubKey(csvEscrowTimeout.hex) == csvEscrowTimeout &&
        CSVEscrowTimeoutScriptPubKey(csvEscrowTimeout.escrow,csvEscrowTimeout.timeout) == csvEscrowTimeout &&
        CSVEscrowTimeoutScriptPubKey.isValidCSVEscrowTimeout(csvEscrowTimeout.asm)
    }
  }
}
