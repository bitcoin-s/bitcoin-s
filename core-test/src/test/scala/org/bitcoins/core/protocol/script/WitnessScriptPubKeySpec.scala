package org.bitcoins.core.protocol.script

import org.bitcoins.testkit.core.gen.ScriptGenerators
import org.scalacheck.{Prop, Properties}

/**
  * Created by chris on 1/3/17.
  */
class WitnessScriptPubKeySpec extends Properties("WitnessScriptPubKeySpec") {

  property("witnessScriptPubKeyV0 serialization symmetry") =
    Prop.forAll(ScriptGenerators.witnessScriptPubKeyV0) {
      case (witScriptPubKeyV0, _) =>
        witScriptPubKeyV0 match {
          case p2wpkh: P2WPKHWitnessSPKV0 =>
            P2WPKHWitnessSPKV0(p2wpkh.hex) == witScriptPubKeyV0
          case p2wsh: P2WSHWitnessSPKV0 =>
            P2WSHWitnessSPKV0(p2wsh.hex) == witScriptPubKeyV0
        }
    }

  property("witnessScriptPubKeyV0 fromAsm symmetry") =
    Prop.forAll(ScriptGenerators.witnessScriptPubKeyV0) {
      case (witScriptPubKeyV0, _) =>
        witScriptPubKeyV0 match {
          case p2wpkh: P2WPKHWitnessSPKV0 =>
            P2WPKHWitnessSPKV0.fromAsm(p2wpkh.asm) == witScriptPubKeyV0
          case p2wsh: P2WSHWitnessSPKV0 =>
            P2WSHWitnessSPKV0.fromAsm(p2wsh.asm) == witScriptPubKeyV0
        }
    }

  property("unassignedWitnessScriptPubKey serialization symmetry") =
    Prop.forAll(ScriptGenerators.unassignedWitnessScriptPubKey) {
      case (unassignedWitScriptPubKey, _) =>
        UnassignedWitnessScriptPubKey(unassignedWitScriptPubKey.hex) == unassignedWitScriptPubKey
    }

  property("unassignedWitnessScriptPubKey fromAsm symmetry") =
    Prop.forAll(ScriptGenerators.unassignedWitnessScriptPubKey) {
      case (unassignedWitScriptPubKey, _) =>
        UnassignedWitnessScriptPubKey.fromAsm(unassignedWitScriptPubKey.asm) == unassignedWitScriptPubKey
    }

  property("witnessScriptPubKey fromAsm symmetry") = {
    Prop.forAll(ScriptGenerators.witnessScriptPubKey) {
      case (witScriptPubKey, _) =>
        WitnessScriptPubKey(witScriptPubKey.asm) == witScriptPubKey
    }
  }
}
