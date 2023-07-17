package org.bitcoins.core.protocol.script

import org.bitcoins.testkitcore.gen.ScriptGenerators
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

class ScriptSpec extends BitcoinSUnitTest {

  it must
    "serialization symmetry for ScriptFactory.fromAsmBytes with ScriptPubKeys" in {
      forAll(ScriptGenerators.scriptPubKey) { case (spk, _) =>
        assert(ScriptPubKey.fromAsmBytes(spk.asmBytes) == spk)
      }
    }

  it must
    "serialization symmetry for ScriptFactory.fromAsmBytes with ScriptSignatures" in {
      forAll(ScriptGenerators.scriptSignature) { case ss =>
        assert(ScriptSignature.fromAsmBytes(ss.asmBytes) == ss)
      }
    }
}
