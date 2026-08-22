package org.bitcoins.core.security

import org.bitcoins.core.crypto.{
  SignatureValidationSuccess,
  TaprootSerializationOptions,
  TaprootTxSigComponent,
  TransactionSignatureChecker,
  TransactionSignatureSerializer,
  WitnessTxSigComponentRaw,
  WitnessTxSigComponentRebuilt
}
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

  // fixed, deterministic key -- no randomness in this spec
  private val privKey1: ECPrivateKey =
    ECPrivateKey.fromFieldElement(FieldElement.one)

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

  it must "commit the segwit v0 sighash to the scriptCode after the last executed OP_CODESEPARATOR" in {
    // Finding (High): the segwit v0 (BIP143) sighash ignores the executed
    // OP_CODESEPARATOR in the scriptCode — checkSignature on a
    // WitnessTxSigComponent ignores the script argument and re-serializes the
    // full witness script
    // (core/src/main/scala/org/bitcoins/core/crypto/TransactionSignatureChecker.scala:160-165,
    // core/src/main/scala/org/bitcoins/core/crypto/TransactionSignatureSerializer.scala:192-243,
    // core/src/main/scala/org/bitcoins/core/util/BitcoinScriptUtil.scala:476-487).
    // Correct behavior: the sighash commits to the script after the last
    // executed OP_CODESEPARATOR, so a signature over that sighash must verify.
    val pubKey = privKey1.publicKey
    // <pubkey> OP_CODESEPARATOR OP_CHECKSIG
    val fullScriptBytes = ByteVector.fromValidHex("21") ++ pubKey.bytes ++
      ByteVector.fromValidHex("abac")
    val witnessScript = ScriptPubKey
      .fromAsmBytes(fullScriptBytes)
      .asInstanceOf[RawScriptPubKey]
    // the scriptCode Core would use: everything after the last OP_CODESEPARATOR
    val strippedScript =
      ScriptPubKey.fromAsmBytes(ByteVector.fromValidHex("ac"))
    val p2wsh = P2WSHWitnessSPKV0(witnessScript)
    val amount = Satoshis(10000)
    val (creditingTx, outputIndex) =
      TransactionTestUtil.buildCreditingTransaction(p2wsh, Some(amount))
    val witness = P2WSHWitnessV0(witnessScript, Vector.empty)
    val (spendingTx, inputIndex) =
      TransactionTestUtil.buildSpendingTransaction(creditingTx,
                                                   EmptyScriptSignature,
                                                   outputIndex,
                                                   Some((witness, amount)))
    val wtx = spendingTx.asInstanceOf[WitnessTransaction]
    val flags = Policy.standardFlags
    val rawComponent =
      WitnessTxSigComponentRaw(wtx,
                               inputIndex,
                               TransactionOutput(amount, p2wsh),
                               flags)
    // manually compute the expected BIP143 sighash with the post-codeseparator
    // scriptCode, and sign that hash
    val expectedHashComponent =
      WitnessTxSigComponentRebuilt(wtx = wtx,
                                   inputIndex = inputIndex,
                                   output =
                                     TransactionOutput(amount, strippedScript),
                                   witScriptPubKey = p2wsh,
                                   flags = flags)
    val expectedHash = TransactionSignatureSerializer.hashForSignature(
      expectedHashComponent,
      HashType.sigHashAll,
      TaprootSerializationOptions.empty)
    val sig =
      privKey1.sign(expectedHash.bytes).appendHashType(HashType.sigHashAll)
    val result = TransactionSignatureChecker.checkSignature(
      txSignatureComponent = rawComponent,
      script = strippedScript.asm,
      pubKey = pubKey.toPublicKeyBytes(),
      signature = sig,
      flags = flags)
    result must be(SignatureValidationSuccess)
  }
}
