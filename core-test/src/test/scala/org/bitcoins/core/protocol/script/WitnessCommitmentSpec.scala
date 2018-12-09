package org.bitcoins.core.protocol.script

import org.bitcoins.core.gen.ScriptGenerators
import org.scalacheck.{Prop, Properties}

/**
  * Created by chris on 1/3/17.
  */
class WitnessCommitmentSpec extends Properties("WitnessCommitmentSpec") {

  property("serialization symmetry") =
    Prop.forAll(ScriptGenerators.witnessCommitment) {
      case (commitment, _) =>
        WitnessCommitment(commitment.hex) == commitment
    }
}
