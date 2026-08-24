package org.bitcoins.core.protocol.script

import org.bitcoins.testkitcore.gen.{ScriptGenerators, WitnessGenerators}
import org.bitcoins.testkitcore.util.BitcoinSUnitTest
import scodec.bits._

import scala.util.Try

class ScriptWitnessSpec extends BitcoinSUnitTest {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    generatorDrivenConfigNewCode

  it must "have serialization symmetry" in {
    forAll(WitnessGenerators.scriptWitness) { scriptWit =>
      val x = ScriptWitness(scriptWit.stack)
      val fromBytes = ScriptWitness.fromBytes(scriptWit.bytes)
      assert(scriptWit == x)
      assert(fromBytes == scriptWit)
    }
  }

  it must "pull redeem script out of p2wsh witness" in {
    forAll(ScriptGenerators.rawScriptPubKey) { case (spk, _) =>
      assert(P2WSHWitnessV0(spk).redeemScript == spk)
    }
  }

  it must "pull script signature out of p2wsh witness" in {
    forAll(
      ScriptGenerators.rawScriptPubKey,
      ScriptGenerators.rawScriptSignature
    ) { case ((spk, _), scriptSig) =>
      assert(P2WSHWitnessV0(spk, scriptSig).scriptSignature == scriptSig)
    }
  }

  it must "reject a witness stack element whose declared length exceeds the remaining bytes" in {
    // RawScriptWitnessParser.read used to clamp the stack element length with
    // ByteVector.take instead of failing, silently truncating witness data.
    // 1 stack item, declared element length 2, only 1 byte available.
    Try(ScriptWitness.fromBytes(hex"0102ff")).isFailure must be(true)
  }

}
