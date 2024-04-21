package org.bitcoins.core.protocol.script

import org.bitcoins.testkitcore.gen.ScriptGenerators
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

/** Created by chris on 1/3/17.
  */
class WitnessScriptPubKeySpec extends BitcoinSUnitTest {

  it must "witnessScriptPubKeyV0 serialization symmetry" in {
    forAll(ScriptGenerators.witnessScriptPubKeyV0) {
      case (witScriptPubKeyV0, _) =>
        witScriptPubKeyV0 match {
          case p2wpkh: P2WPKHWitnessSPKV0 =>
            assert(P2WPKHWitnessSPKV0(p2wpkh.hex) == witScriptPubKeyV0)
          case p2wsh: P2WSHWitnessSPKV0 =>
            assert(P2WSHWitnessSPKV0(p2wsh.hex) == witScriptPubKeyV0)
        }
    }
  }

  it must "witnessScriptPubKeyV0 fromAsm symmetry" in {
    forAll(ScriptGenerators.witnessScriptPubKeyV0) {
      case (witScriptPubKeyV0, _) =>
        witScriptPubKeyV0 match {
          case p2wpkh: P2WPKHWitnessSPKV0 =>
            assert(P2WPKHWitnessSPKV0.fromAsm(p2wpkh.asm) == witScriptPubKeyV0)
          case p2wsh: P2WSHWitnessSPKV0 =>
            assert(P2WSHWitnessSPKV0.fromAsm(p2wsh.asm) == witScriptPubKeyV0)
        }
    }
  }

  it must "unassignedWitnessScriptPubKey serialization symmetry" in {
    forAll(ScriptGenerators.unassignedWitnessScriptPubKey) {
      case (unassignedWitScriptPubKey, _) =>
        assert(
          UnassignedWitnessScriptPubKey(
            unassignedWitScriptPubKey.hex
          ) == unassignedWitScriptPubKey
        )
    }
  }

  it must "unassignedWitnessScriptPubKey fromAsm symmetry" in {
    forAll(ScriptGenerators.unassignedWitnessScriptPubKey) {
      case (unassignedWitScriptPubKey, _) =>
        assert(
          UnassignedWitnessScriptPubKey.fromAsm(
            unassignedWitScriptPubKey.asm
          ) == unassignedWitScriptPubKey
        )
    }
  }

  it must "witnessScriptPubKey fromAsm symmetry" in {
    forAll(ScriptGenerators.witnessScriptPubKey) { case (witScriptPubKey, _) =>
      assert(WitnessScriptPubKey(witScriptPubKey.asm) == witScriptPubKey)
    }
  }
}
