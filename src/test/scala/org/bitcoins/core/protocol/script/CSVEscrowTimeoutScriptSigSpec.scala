package org.bitcoins.core.protocol.script

import org.bitcoins.core.gen.ScriptGenerators
import org.bitcoins.core.util.BitcoinSLogger
import org.scalacheck.{Prop, Properties}

/**
  * Created by chris on 3/28/17.
  */
class CSVEscrowTimeoutScriptSigSpec extends Properties("CSVEscrowWithTimeoutScriptSigSpec") with BitcoinSLogger  {

  property("serialization symmetry") =
    Prop.forAll(ScriptGenerators.csvEscrowTimeoutScriptSig) { scriptSig =>
      CSVEscrowTimeoutScriptSignature(scriptSig.hex) == scriptSig &&
      CSVEscrowTimeoutScriptSignature.fromAsm(scriptSig.asm) == scriptSig
    }
}
