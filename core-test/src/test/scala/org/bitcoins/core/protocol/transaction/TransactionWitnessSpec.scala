package org.bitcoins.core.protocol.transaction

import org.bitcoins.core.gen.WitnessGenerators
import org.scalacheck.{Prop, Properties}

/**
  * Created by chris on 11/28/16.
  */
class TransactionWitnessSpec extends Properties("TransactionWitnessSpec") {

  property("serialization symmetry") = {
    Prop.forAll(WitnessGenerators.transactionWitness) { witness =>
      TransactionWitness(witness.hex, witness.witnesses.size) == witness
    }
  }
}
