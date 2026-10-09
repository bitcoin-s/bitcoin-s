package org.bitcoins.core.protocol.script

import org.bitcoins.crypto.{ECDigitalSignature, ECPublicKey}
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

class ScriptWitnessV0Test extends BitcoinSUnitTest {
  val uncompressed: ECPublicKey = ECPublicKey.freshPublicKey.decompressed
  val p2pk: P2PKScriptPubKey = P2PKScriptPubKey(uncompressed)

  "P2WPKHWitnessV0" must "fail to be created with an uncompressed public key" in {
    intercept[IllegalArgumentException] {
      P2WPKHWitnessV0(uncompressed)
    }
    intercept[IllegalArgumentException] {
      P2WPKHWitnessV0(uncompressed, ECDigitalSignature.dummy)
    }
  }

  it must "still parse a witness with an uncompressed public key" in {
    val stack = Vector(uncompressed.bytes, ECDigitalSignature.dummy.bytes)
    val witness = ScriptWitness(stack)
    assert(witness.isInstanceOf[P2WPKHWitnessV0])
    assert(witness.stack == stack)
    assert(ScriptWitness.fromBytes(witness.bytes) == witness)
  }

  "P2WSHWitnessV0" must "fail to be created with an uncompressed public key" in {
    intercept[IllegalArgumentException] {
      P2WSHWitnessV0(p2pk)
    }
    intercept[IllegalArgumentException] {
      P2WSHWitnessV0(p2pk, Vector(ECDigitalSignature.dummy.bytes))
    }
  }

  it must "still parse a witness with an uncompressed public key" in {
    val stack = Vector(p2pk.asmBytes, ECDigitalSignature.dummy.bytes)
    val witness = ScriptWitness(stack)
    assert(witness.isInstanceOf[P2WSHWitnessV0])
    assert(witness.stack == stack)
    assert(ScriptWitness.fromBytes(witness.bytes) == witness)
  }
}
