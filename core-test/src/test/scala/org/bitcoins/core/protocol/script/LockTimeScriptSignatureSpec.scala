package org.bitcoins.core.protocol.script

import org.bitcoins.testkit.core.gen.{NumberGenerator, ScriptGenerators}
import org.scalacheck.{Prop, Properties}

class LockTimeScriptSignatureSpec
    extends Properties("LockTimeScriptSignature") {
  property("Nesting agreement") =
    Prop.forAll(ScriptGenerators.scriptSignature) { scriptSig =>
      val csvScriptSig = CSVScriptSignature(scriptSig)
      val cltvScriptSig = CLTVScriptSignature(scriptSig)

      csvScriptSig.scriptSig == scriptSig && cltvScriptSig.scriptSig == scriptSig
    }

  property("Signature agreement") =
    Prop.forAll(ScriptGenerators.lockTimeScriptSig) { lockTimeScriptSignature =>
      lockTimeScriptSignature.signatures == lockTimeScriptSignature.scriptSig.signatures
    }
}
