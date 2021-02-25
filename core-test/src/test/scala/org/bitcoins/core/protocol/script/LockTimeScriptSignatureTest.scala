package org.bitcoins.core.protocol.script

import org.bitcoins.testkitcore.gen.ScriptGenerators
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

class LockTimeScriptSignatureTest extends BitcoinSUnitTest {
  behavior of "LockTimeScriptSignature"

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration = {
    generatorDrivenConfigNewCode
  }

  it should "have agreement with nesting ScriptSignatures" in {
    forAll(ScriptGenerators.scriptSignature) { scriptSig =>
      val csvScriptSig = CSVScriptSignature(scriptSig)
      val cltvScriptSig = CLTVScriptSignature(scriptSig)

      assert(csvScriptSig.scriptSig == scriptSig)
      assert(cltvScriptSig.scriptSig == scriptSig)
    }
  }

  it should "have agreement with nested signatures" in {
    forAll(ScriptGenerators.lockTimeScriptSig) { lockTimeScriptSignature =>
      assert(
        lockTimeScriptSignature.signatures == lockTimeScriptSignature.scriptSig.signatures)
    }
  }
}
