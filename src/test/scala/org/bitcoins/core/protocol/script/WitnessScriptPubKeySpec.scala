package org.bitcoins.core.protocol.script

import org.bitcoins.core.gen.ScriptGenerators
import org.scalacheck.{Prop, Properties}

/**
  * Created by chris on 1/3/17.
  */
class WitnessScriptPubKeySpec extends Properties("WitnessScriptPubKeySpec") {

  property("witnessScriptPubKeyV0 serialization symmetry") =
    Prop.forAll(ScriptGenerators.witnessScriptPubKeyV0) { case (witScriptPubKeyV0, _) =>
        WitnessScriptPubKeyV0(witScriptPubKeyV0.hex) == witScriptPubKeyV0
    }

  property("witnessScriptPubKeyV0 fromAsm symmetry") =
    Prop.forAll(ScriptGenerators.witnessScriptPubKeyV0) { case (witScriptPubKeyV0,_) =>
        WitnessScriptPubKeyV0.fromAsm(witScriptPubKeyV0.asm) == witScriptPubKeyV0
    }

  property("unassignedWitnessScriptPubKey serialization symmetry") =
    Prop.forAll(ScriptGenerators.unassignedWitnessScriptPubKey) { case (unassignedWitScriptPubKey,_) =>
        UnassignedWitnessScriptPubKey(unassignedWitScriptPubKey.hex) == unassignedWitScriptPubKey
    }

  property("unassignedWitnessScriptPubKey fromAsm symmetry") =
    Prop.forAll(ScriptGenerators.unassignedWitnessScriptPubKey) { case (unassignedWitScriptPubKey,_) =>
        UnassignedWitnessScriptPubKey.fromAsm(unassignedWitScriptPubKey.asm) == unassignedWitScriptPubKey
    }

  property("witnessScriptPubKey fromAsm symmetry") = {
    Prop.forAll(ScriptGenerators.witnessScriptPubKey) { case (witScriptPubKey,_) =>
        WitnessScriptPubKey(witScriptPubKey.asm).get == witScriptPubKey
    }
  }
}
