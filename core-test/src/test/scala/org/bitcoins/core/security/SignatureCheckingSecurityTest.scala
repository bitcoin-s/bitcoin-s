package org.bitcoins.core.security

import org.bitcoins.core.crypto.TaprootTxSigComponent
import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script.*
import org.bitcoins.core.protocol.transaction.*
import org.bitcoins.core.script.PreExecutionScriptProgram
import org.bitcoins.core.script.flag.ScriptVerifyDiscourageUpgradableWitnessProgram
import org.bitcoins.core.script.interpreter.ScriptInterpreter
import org.bitcoins.core.script.result.ScriptOk
import org.bitcoins.core.script.util.PreviousOutputMap
import org.bitcoins.crypto.*
import org.bitcoins.testkitcore.util.{BitcoinSUnitTest, TransactionTestUtil}
import scodec.bits.ByteVector

import scala.util.{Failure, Success, Try}

/** Security reproduction tests for sighash and signature verification
  * semantics. Each test asserts the CORRECT/SAFE behavior (Bitcoin Core and BIP
  * parity), so every test here FAILS until the underlying bug is fixed. The
  * failing tests are the fix checklist.
  */
class SignatureCheckingSecurityTest extends BitcoinSUnitTest {

  behavior of "Signature checking security"

  it must "not treat a v1 witness program with an invalid x coordinate as anyone-can-spend" in {
    // Finding (High): a taproot (v1) 32-byte witness program whose x-only key
    // is NOT a valid x coordinate is misclassified as an unassigned witness
    // program, making it anyone-can-spend
    // (core/src/main/scala/org/bitcoins/core/protocol/script/ScriptPubKey.scala:1491-1499,
    // core/src/main/scala/org/bitcoins/core/script/interpreter/ScriptInterpreter.scala:586-604).
    // Correct behavior: the program is recognized as taproot and validation fails.
    // 0xffff...ff >= secp256k1 field size p, so it cannot be a valid x coordinate
    val invalidProgram = ByteVector.fill(32)(0xff.toByte)
    val spkBytes = ByteVector.fromValidHex("5120") ++ invalidProgram
    Try(ScriptPubKey.fromAsmBytes(spkBytes)) match {
      case Failure(_) =>
        // rejecting the invalid taproot output key at parse time is acceptable
        succeed
      case Success(spk) =>
        assert(
          !spk.isInstanceOf[UnassignedWitnessScriptPubKey],
          s"A v1 32-byte witness program must be classified as taproot, not as an unassigned witness program, got=$spk"
        )
        val amount = Satoshis(10000)
        val (creditingTx, outputIndex) =
          TransactionTestUtil.buildCreditingTransaction(spk, Some(amount))
        val witness =
          ScriptWitness(Vector(SchnorrDigitalSignature.dummy.bytes))
        val (spendingTx, inputIndex) =
          TransactionTestUtil.buildSpendingTransaction(creditingTx,
                                                       EmptyScriptSignature,
                                                       outputIndex,
                                                       Some((witness, amount)))
        val flags = Policy.standardFlags.filterNot(
          _ == ScriptVerifyDiscourageUpgradableWitnessProgram)
        val wtx = spendingTx.asInstanceOf[WitnessTransaction]
        val outpoint = wtx.inputs(inputIndex.toInt).previousOutput
        val outputMap =
          PreviousOutputMap(Map(outpoint -> TransactionOutput(amount, spk)))
        val component =
          TaprootTxSigComponent(wtx, inputIndex, outputMap, flags)
        val result = ScriptInterpreter.run(PreExecutionScriptProgram(component))
        assert(
          result != ScriptOk,
          s"Spending a v1 witness program with an invalid x-only key must fail validation, got=$result")
    }
  }
}
