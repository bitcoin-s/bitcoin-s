package org.bitcoins.core.protocol.transaction

import org.bitcoins.core.crypto.{ECPrivateKey, EmptyDigitalSignature}
import org.bitcoins.core.gen.WitnessGenerators
import org.bitcoins.core.protocol.script._
import org.scalacheck.Prop
import org.scalatest.{FlatSpec, MustMatchers}

/**
  * Created by chris on 11/28/16.
  */
class TransactionWitnessSpec extends FlatSpec with MustMatchers {

  behavior of "TransactionWitness"

  it must "have serialization symmetry" in {
    Prop.forAll(WitnessGenerators.transactionWitness) { witness =>
      TransactionWitness(witness.hex, witness.witnesses.size) == witness
    }
  }

  it must "be able to resize a witness to the given index" in {
    val empty = EmptyWitness
    val pubKey = ECPrivateKey.freshPrivateKey.publicKey
    val p2pkh = P2PKHScriptSignature(EmptyDigitalSignature,pubKey)
    val scriptWit = P2WPKHWitnessV0.fromP2PKHScriptSig(p2pkh)
    val updated = empty.updated(2,scriptWit)

    updated.witnesses.length must be (3)
  }


  it must "fail to update a negative index witness" in {
    intercept[IndexOutOfBoundsException] {
      EmptyWitness.updated(-1,EmptyScriptWitness)
    }
  }

}
