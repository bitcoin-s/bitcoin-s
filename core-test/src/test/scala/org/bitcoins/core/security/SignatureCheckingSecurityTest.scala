package org.bitcoins.core.security

import org.bitcoins.core.crypto.{
  BaseTxSigComponent,
  SignatureValidationErrorIncorrectSignatures,
  SignatureValidationSuccess,
  TaprootSerializationOptions,
  TaprootTxSigComponent,
  TransactionSignatureChecker,
  TransactionSignatureSerializer,
  WitnessTxSigComponentRaw,
  WitnessTxSigComponentRebuilt
}
import org.bitcoins.core.currency.{CurrencyUnits, Satoshis}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script.*
import org.bitcoins.core.protocol.transaction.*
import org.bitcoins.core.script.PreExecutionScriptProgram
import org.bitcoins.core.script.constant.{OP_0, ScriptConstant}
import org.bitcoins.core.script.flag.{
  ScriptFlag,
  ScriptVerifyDiscourageUpgradableWitnessProgram,
  ScriptVerifyLowS,
  ScriptVerifyNullFail
}
import org.bitcoins.core.script.interpreter.ScriptInterpreter
import org.bitcoins.core.script.result.{
  ScriptErrorSchnorrSigHashType,
  ScriptErrorSigNullFail,
  ScriptOk
}
import org.bitcoins.core.script.util.PreviousOutputMap
import org.bitcoins.core.util.BitcoinScriptUtil
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

  // fixed, deterministic keys — no randomness in this spec
  private val privKey1: ECPrivateKey =
    ECPrivateKey.fromFieldElement(FieldElement.one)
  private val privKey2: ECPrivateKey =
    ECPrivateKey.fromBytes(ByteVector.fill(32)(2.toByte))
  private val noncePrivKey: ECPrivateKey =
    ECPrivateKey.fromBytes(ByteVector.fill(32)(3.toByte))

  /** Builds a single input taproot script path spend of a single tapleaf.
    * Returns the sig component and the tapleaf hash so callers can compute the
    * BIP341 sighash themselves.
    */
  private def buildTapscriptSpend(
      leafScriptBytes: ByteVector,
      witnessStack: Vector[ByteVector],
      flags: Seq[ScriptFlag]): (TaprootTxSigComponent, Sha256Digest) = {
    val leafSPK =
      ScriptPubKey.fromAsmBytes(leafScriptBytes).asInstanceOf[RawScriptPubKey]
    val internalKey = privKey2.toXOnly
    val leaf = TapLeaf(LeafVersion.Tapscript, leafSPK)
    val (keyParity, taprootSPK) =
      TaprootScriptPubKey.fromInternalKeyTapscriptTree(internalKey, leaf)
    val controlBlock =
      TapscriptControlBlock.fromSingleLeaf(LeafVersion.Tapscript,
                                           internalKey,
                                           keyParity)
    val witness = TaprootScriptPath(controlBlock, None, leafSPK, witnessStack)
    val amount = Satoshis(10000)
    val (creditingTx, outputIndex) =
      TransactionTestUtil.buildCreditingTransaction(taprootSPK, Some(amount))
    val (spendingTx, inputIndex) =
      TransactionTestUtil.buildSpendingTransaction(creditingTx,
                                                   EmptyScriptSignature,
                                                   outputIndex,
                                                   Some((witness, amount)))
    val wtx = spendingTx.asInstanceOf[WitnessTransaction]
    val outpoint = wtx.inputs(inputIndex.toInt).previousOutput
    val outputMap = PreviousOutputMap(
      Map(outpoint -> creditingTx.outputs(outputIndex.toInt)))
    val component = TaprootTxSigComponent(wtx, inputIndex, outputMap, flags)
    val tapLeafHash = TaprootScriptPath.computeTapleafHash(leaf)
    (component, tapLeafHash)
  }

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

  it must "reject a 65-byte tapscript signature with an explicit SIGHASH_DEFAULT byte" in {
    // Finding (Medium): tapscript accepts a 65-byte signature whose last byte
    // is 0x00 (explicit SIGHASH_DEFAULT), violating BIP341
    // (core/src/main/scala/org/bitcoins/core/script/crypto/CryptoInterpreter.scala:157-172,228-240,
    // crypto/src/main/scala/org/bitcoins/crypto/HashType.scala:205-216).
    // Correct behavior: validation fails with ScriptErrorSchnorrSigHashType.
    val flags = Policy.standardFlags
    // <x-only pubkey> OP_CHECKSIG
    val leafScriptBytes = ByteVector.fromValidHex("20") ++
      privKey1.toXOnly.bytes ++ ByteVector.fromValidHex("ac")
    // build a placeholder spend to compute the BIP341 sighash, then sign it
    val (placeholderComponent, tapLeafHash) =
      buildTapscriptSpend(leafScriptBytes, Vector.empty, flags)
    val sighash = TransactionSignatureSerializer.hashForSignature(
      placeholderComponent,
      HashType.sigHashDefault,
      TaprootSerializationOptions(Some(tapLeafHash), None, None))
    val sig64 = privKey1.schnorrSignWithNonce(sighash.bytes, noncePrivKey)
    // explicitly append SIGHASH_DEFAULT (0x00) — not allowed by BIP341
    val sig65 = sig64.bytes ++ ByteVector.fromByte(0x00.toByte)
    val (component, _) =
      buildTapscriptSpend(leafScriptBytes, Vector(sig65), flags)
    val result = ScriptInterpreter.run(PreExecutionScriptProgram(component))
    result must be(ScriptErrorSchnorrSigHashType)
  }

  it must "fail validation for taproot SIGHASH_SINGLE with no corresponding output" in {
    // Finding (Medium): taproot SIGHASH_SINGLE with a missing corresponding
    // output returns the legacy uint256-one hash instead of failing validation
    // (core/src/main/scala/org/bitcoins/core/crypto/TransactionSignatureSerializer.scala:398-409).
    // Correct behavior per BIP341: validation fails.
    val taprootSPK = TaprootScriptPubKey(privKey1.toXOnly)
    val amount1 = Satoshis(10000)
    val amount2 = Satoshis(20000)
    val (creditingTx1, _) =
      TransactionTestUtil.buildCreditingTransaction(taprootSPK, Some(amount1))
    val (creditingTx2, _) =
      TransactionTestUtil.buildCreditingTransaction(taprootSPK, Some(amount2))
    val outpoint1 = TransactionOutPoint(creditingTx1.txId, UInt32.zero)
    val outpoint2 = TransactionOutPoint(creditingTx2.txId, UInt32.zero)
    val inputs = Vector(
      TransactionInput(outpoint1,
                       EmptyScriptSignature,
                       TransactionConstants.sequence),
      TransactionInput(outpoint2,
                       EmptyScriptSignature,
                       TransactionConstants.sequence)
    )
    // only one output, so input index 1 has no corresponding output
    val outputs =
      Vector(TransactionOutput(Satoshis(5000), EmptyScriptPubKey))
    val witness =
      TransactionWitness(Vector(TaprootKeyPath.dummy, TaprootKeyPath.dummy))
    val wtx = WitnessTransaction(
      TransactionConstants.version,
      inputs,
      outputs,
      TransactionConstants.lockTime,
      witness
    )
    val outputMap = PreviousOutputMap(
      Map(outpoint1 -> creditingTx1.outputs.head,
          outpoint2 -> creditingTx2.outputs.head))
    val component =
      TaprootTxSigComponent(wtx, UInt32.one, outputMap, Policy.standardFlags)
    val hashT = Try(
      TransactionSignatureSerializer.hashForSignature(
        component,
        HashType.sigHashSingle,
        TaprootSerializationOptions.empty))
    assert(
      hashT.isFailure,
      s"BIP341 requires taproot SIGHASH_SINGLE with no corresponding output to fail validation, got=$hashT")
  }

  it must "fail a tapscript OP_CHECKSIG immediately on an invalid non-empty signature" in {
    // Finding (Medium): tapscript OP_CHECKSIG with an invalid non-empty
    // signature pushes 0 and continues when NULLFAIL is not set
    // (core/src/main/scala/org/bitcoins/core/script/crypto/CryptoInterpreter.scala:129-135,
    // core/src/main/scala/org/bitcoins/core/crypto/TransactionSignatureChecker.scala:200-220,317-326).
    // Correct behavior per BIP342 (and Core, which fails with
    // SCRIPT_ERR_SCHNORR_SIG): script execution fails immediately.
    val flags = Policy.standardFlags.filterNot(_ == ScriptVerifyNullFail)
    // <x-only pubkey> OP_CHECKSIG OP_DROP OP_TRUE
    // if an invalid signature only pushed 0, this script would succeed
    val leafScriptBytes = ByteVector.fromValidHex("20") ++
      privKey1.toXOnly.bytes ++ ByteVector.fromValidHex("ac7551")
    // a well-formed 64-byte schnorr signature over a different message, so it
    // is invalid for this input's sighash
    val invalidSig =
      privKey1.schnorrSignWithNonce(ByteVector.fill(32)(0x55.toByte),
                                    noncePrivKey)
    val (component, _) =
      buildTapscriptSpend(leafScriptBytes, Vector(invalidSig.bytes), flags)
    val result = ScriptInterpreter.run(PreExecutionScriptProgram(component))
    assert(
      result != ScriptOk,
      s"An invalid non-empty tapscript signature must fail the script immediately, got=$result")
  }

  it must "not fail an empty signature with the LOW_S check" in {
    // Finding (Medium): an empty signature combined with the LOW_S flag fails
    // the whole script with ScriptErrorSigHighS instead of just pushing false
    // (core/src/main/scala/org/bitcoins/core/crypto/TransactionSignatureChecker.scala:133-136).
    // Correct behavior: Core's CheckSignatureEncoding exempts the empty
    // signature from all encoding checks, so verification just returns false.
    val pubKey = privKey1.publicKey
    val spk = P2PKHScriptPubKey(pubKey)
    val (creditingTx, outputIndex) =
      TransactionTestUtil.buildCreditingTransaction(spk)
    val (spendingTx, inputIndex) =
      TransactionTestUtil.buildSpendingTransaction(creditingTx,
                                                   EmptyScriptSignature,
                                                   outputIndex)
    val component =
      BaseTxSigComponent(spendingTx,
                         inputIndex,
                         TransactionOutput(CurrencyUnits.zero, spk),
                         Seq(ScriptVerifyLowS))
    val result = TransactionSignatureChecker.checkSignature(
      txSignatureComponent = component,
      script = spk.asm,
      pubKey = pubKey.toPublicKeyBytes(),
      signature = ECDigitalSignature.empty,
      flags = Seq(ScriptVerifyLowS))
    result must be(SignatureValidationErrorIncorrectSignatures)
  }

  it must "apply NULLFAIL to all signatures in OP_CHECKMULTISIG, including consumed ones" in {
    // Finding (Medium): NULLFAIL in OP_CHECKMULTISIG only checks the
    // unconsumed signatures, not all provided ones
    // (core/src/main/scala/org/bitcoins/core/crypto/TransactionSignatureChecker.scala:267-298).
    // Correct behavior: Core checks every signature slot on failure, so a
    // failing CHECKMULTISIG with any non-empty signature must fail the script.
    // LOW_S is removed to decouple this test from the empty-signature LOW_S bug
    val flags = Policy.standardFlags.filterNot(_ == ScriptVerifyLowS)
    val pubKey1 = privKey1.publicKey
    val pubKey2 = privKey2.publicKey
    val multiSigSPK = MultiSignatureScriptPubKey(2, Vector(pubKey1, pubKey2))
    val amount = Satoshis(10000)
    val (creditingTx, outputIndex) =
      TransactionTestUtil.buildCreditingTransaction(multiSigSPK, Some(amount))
    val (placeholderTx, inputIndex) =
      TransactionTestUtil.buildSpendingTransaction(creditingTx,
                                                   EmptyScriptSignature,
                                                   outputIndex)
    val output = TransactionOutput(amount, multiSigSPK)
    val placeholderComponent =
      BaseTxSigComponent(placeholderTx, inputIndex, output, flags)
    val hash = TransactionSignatureSerializer.hashForSignature(
      placeholderComponent,
      HashType.sigHashAll,
      TaprootSerializationOptions.empty)
    // pubKey2 is pushed last in the multisig script, so it is checked first
    val validSig =
      privKey2.sign(hash.bytes).appendHashType(HashType.sigHashAll)
    val sigPush = BitcoinScriptUtil.calculatePushOp(validSig.bytes) ++ Vector(
      ScriptConstant(validSig.bytes))
    // dummy, empty signature, valid signature — the valid signature is on top
    // of the stack and is checked (and consumed) first
    val scriptSig =
      NonStandardScriptSignature.fromAsm(Vector(OP_0, OP_0) ++ sigPush)
    val (spendingTx, _) =
      TransactionTestUtil.buildSpendingTransaction(creditingTx,
                                                   scriptSig,
                                                   outputIndex)
    val component = BaseTxSigComponent(spendingTx, inputIndex, output, flags)
    val result = ScriptInterpreter.run(PreExecutionScriptProgram(component))
    result must be(ScriptErrorSigNullFail)
  }
}
