package org.bitcoins.core.wallet.signer

import org.bitcoins.core.crypto._
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.core.psbt.PSBT
import org.bitcoins.core.script.flag.ScriptFlag
import org.bitcoins.core.wallet.builder.TxBuilderError
import org.bitcoins.core.wallet.utxo._
import org.bitcoins.crypto.{HashType, _}
import scodec.bits.ByteVector

sealed abstract class SignerUtils {

  def doSign[Sig <: DigitalSignature](
      unsignedTx: Transaction,
      signingInfo: InputSigningInfo[InputInfo],
      sign: (ByteVector, HashType) => Sig,
      hashType: HashType): Sig = {
    TransactionSignatureCreator.createSig(unsignedTx,
                                          signingInfo,
                                          sign,
                                          hashType)
  }

  def signSingle[Sig <: DigitalSignature](
      spendingInfo: ECSignatureParams[InputInfo],
      unsignedTx: Transaction,
      signWithHashType: (ByteVector, HashType) => Sig)
      : PartialSignature[Sig] = {

    val tx = spendingInfo.inputInfo match {
      case _: SegwitV0NativeInputInfo | _: P2SHNestedSegwitV0InputInfo |
          _: UnassignedSegwitNativeInputInfo | _: TaprootKeyPathInputInfo =>
        TxUtil.addWitnessData(unsignedTx, spendingInfo)
      case _: RawInputInfo | _: P2SHNonSegwitInputInfo =>
        unsignedTx
    }

    val signature: Sig = doSign(
      unsignedTx = tx,
      signingInfo = spendingInfo,
      sign = signWithHashType,
      hashType = spendingInfo.hashType
    )

    PartialSignature(spendingInfo.signer.publicKey.toPublicKeyBytes(),
                     signature)
  }

  protected val flags: Seq[ScriptFlag] = Policy.standardFlags

  protected def relevantInfo(
      spendingInfo: InputSigningInfo[InputInfo],
      unsignedTx: Transaction)
      : (Seq[Sign], TransactionOutput, UInt32, HashType) = {
    (spendingInfo.signers,
     spendingInfo.output,
     inputIndex(spendingInfo, unsignedTx),
     spendingInfo.hashType)
  }

  protected def inputIndex(
      spendingInfo: InputSigningInfo[InputInfo],
      tx: Transaction): UInt32 = {
    tx.inputs.zipWithIndex
      .find(_._1.previousOutput == spendingInfo.outPoint) match {
      case Some((_, index)) => UInt32(index)
      case None =>
        throw new IllegalArgumentException(
          "Transaction did not contain expected input.")
    }
  }
}

/** The class used to represent a signing process for a specific
  * [[org.bitcoins.core.protocol.script.ScriptPubKey]] type
  */
sealed abstract class Signer[-InputType <: InputInfo] extends SignerUtils {

  /** The method used to sign a bitcoin unspent transaction output
    * @param spendingInfo
    *   \- The information required for signing
    * @param unsignedTx
    *   the external Transaction that needs an input signed
    * @return
    */
  def sign(
      spendingInfo: ScriptSignatureParams[InputType],
      unsignedTx: Transaction): TxSigComponent = {
    sign(
      spendingInfo,
      unsignedTx,
      spendingInfoToSatisfy = spendingInfo
    )
  }

  /** The method used to sign a bitcoin unspent transaction output that is
    * potentially nested
    * @param spendingInfo
    *   \- The information required for signing
    * @param unsignedTx
    *   the external Transaction that needs an input signed
    * @param spendingInfoToSatisfy
    *   \- specifies the NewSpendingInfo whose ScriptPubKey needs a
    *   ScriptSignature to be generated
    * @return
    */
  def sign(
      spendingInfo: ScriptSignatureParams[InputInfo],
      unsignedTx: Transaction,
      spendingInfoToSatisfy: ScriptSignatureParams[InputType]): TxSigComponent

  /** Creates a BaseTxSigComponent by replacing the unsignedTx input at
    * inputIndex with a signed one using the given ScriptSignature
    */
  protected def updateScriptSigInSigComponent(
      unsignedTx: Transaction,
      inputIndex: Int,
      output: TransactionOutput,
      scriptSignature: ScriptSignature): BaseTxSigComponent = {
    val unsignedInput = unsignedTx.inputs(inputIndex)

    val signedInput = TransactionInput(unsignedInput.previousOutput,
                                       scriptSignature,
                                       unsignedInput.sequence)
    val signedInputs = unsignedTx.inputs.updated(inputIndex, signedInput)
    val signedTx = unsignedTx match {
      case btx: NonWitnessTransaction =>
        BaseTransaction(btx.version, signedInputs, btx.outputs, btx.lockTime)
      case wtx: WitnessTransaction =>
        WitnessTransaction(wtx.version,
                           signedInputs,
                           wtx.outputs,
                           wtx.lockTime,
                           wtx.witness)
    }

    BaseTxSigComponent(signedTx, UInt32(inputIndex), output, flags)
  }
}

object BitcoinSigner extends SignerUtils {

  def sign(
      spendingInfo: ScriptSignatureParams[InputInfo],
      unsignedTx: Transaction): TxSigComponent = {
    sign(spendingInfo, unsignedTx, spendingInfo)
  }

  def sign(
      spendingInfo: ScriptSignatureParams[InputInfo],
      unsignedTx: Transaction,
      spendingInfoToSatisfy: ScriptSignatureParams[InputInfo])
      : TxSigComponent = {
    def spendingFrom[Info <: InputInfo](
        inputInfo: Info): ScriptSignatureParams[Info] = {
      spendingInfoToSatisfy.copy(inputInfo = inputInfo)
    }

    spendingInfoToSatisfy.inputInfo match {
      case empty: EmptyInputInfo =>
        EmptySigner.sign(spendingInfo, unsignedTx, spendingFrom(empty))
      case p2pk: P2PKInputInfo =>
        P2PKSigner.sign(spendingInfo, unsignedTx, spendingFrom(p2pk))
      case p2pkh: P2PKHInputInfo =>
        P2PKHSigner.sign(spendingInfo, unsignedTx, spendingFrom(p2pkh))
      case p2pKWithTimeout: P2PKWithTimeoutInputInfo =>
        P2PKWithTimeoutSigner.sign(spendingInfo,
                                   unsignedTx,
                                   spendingFrom(p2pKWithTimeout))
      case p2sh: P2SHInputInfo =>
        P2SHSigner.sign(spendingInfo, unsignedTx, spendingFrom(p2sh))
      case multiSig: MultiSignatureInputInfo =>
        MultiSigSigner.sign(spendingInfo, unsignedTx, spendingFrom(multiSig))
      case lockTime: LockTimeInputInfo =>
        LockTimeSigner.sign(spendingInfo, unsignedTx, spendingFrom(lockTime))
      case conditional: ConditionalInputInfo =>
        ConditionalSigner.sign(spendingInfo,
                               unsignedTx,
                               spendingFrom(conditional))
      case p2wpkh: P2WPKHV0InputInfo =>
        P2WPKHSigner.sign(spendingInfo, unsignedTx, spendingFrom(p2wpkh))
      case pw2sh: P2WSHV0InputInfo =>
        P2WSHSigner.sign(spendingInfo, unsignedTx, spendingFrom(pw2sh))
      case trk: TaprootKeyPathInputInfo =>
        TaprootKeyPathSigner.sign(spendingInfo,
                                  unsignedTx,
                                  spendingInfoToSatisfy = spendingFrom(trk))
      case _: UnassignedSegwitNativeInputInfo =>
        throw new UnsupportedOperationException("Unsupported Segwit version")
    }
  }

  /** Signs the PSBT's input at the given input with the signer, then adds it to
    * the PSBT in a PartialSignature record
    * @param psbt
    *   The PSBT to sign
    * @param inputIndex
    *   Index of input to sign
    * @param signer
    *   Function or private key used to sign the PSBT
    * @param conditionalPath
    *   Represents the spending branch being taken in a ScriptPubKey's execution
    * @return
    */
  def sign(
      psbt: PSBT,
      inputIndex: Int,
      signer: Sign,
      conditionalPath: ConditionalPath = ConditionalPath.NoCondition): PSBT = {
    // if already signed by this signer
    val partialSigs = psbt
      .inputMaps(inputIndex)
      .partialSignatures
    val isSigned = partialSigs.exists(_.pubKey.toPublicKey == signer.publicKey)
    if (isSigned) {
      throw new IllegalArgumentException(
        "Input has already been signed with this key")
    }

    val tx = psbt.transaction
    val spendingInfo =
      psbt
        .inputMaps(inputIndex)
        .toUTXOSigningInfo(tx.inputs(inputIndex), signer, conditionalPath)

    val partialSignature = spendingInfo.output.scriptPubKey match {
      case _: WitnessScriptPubKeyV0 =>
        val txToSign = tx match {
          case btx: NonWitnessTransaction =>
            val witnesses = psbt.inputMaps.map { map =>
              map.witnessScriptOpt.map(scriptWit =>
                P2WSHWitnessV0(scriptWit.witnessScript))
            }
            val transactionWitness = TransactionWitness.fromWitOpt(witnesses)

            WitnessTransaction(btx.version,
                               btx.inputs,
                               btx.outputs,
                               btx.lockTime,
                               transactionWitness)
          case wtx: WitnessTransaction =>
            wtx.witness(inputIndex) match {
              case EmptyScriptWitness =>
                val transactionWitnessOpt =
                  psbt
                    .inputMaps(inputIndex)
                    .witnessScriptOpt
                    .map(scriptWit => P2WSHWitnessV0(scriptWit.witnessScript))
                transactionWitnessOpt match {
                  case Some(wit) => wtx.updateWitness(inputIndex, wit)
                  case None      => wtx
                }
              case _: P2WPKHWitnessV0 | _: P2WSHWitnessV0 | _: TaprootWitness =>
                wtx
            }
        }
        signSingle(spendingInfo, txToSign, signer.signLowRWithHashType)
      case _: TaprootScriptPubKey =>
        signSingle(spendingInfo, tx, signer.schnorrSignWithHashType)
      case _: NonWitnessScriptPubKey =>
        signSingle(spendingInfo, tx, signer.signLowRWithHashType)
      case u: UnassignedWitnessScriptPubKey =>
        sys.error(s"Cannot sign unsupported witSPK=$u")
    }

    psbt.addSignature(partialSignature, inputIndex)
  }
}

/** Represents a SingleKeyBitcoinSigner which signs a RawScriptPubKey */
sealed abstract class RawSingleKeyBitcoinSigner[-InputType <: RawInputInfo]
    extends Signer[InputType] {

  def keyAndSigToScriptSig(
      key: ECPublicKey,
      sig: ECDigitalSignature,
      spendingInfo: InputSigningInfo[InputType]): ScriptSignature

  override def sign(
      spendingInfo: ScriptSignatureParams[InputInfo],
      unsignedTx: Transaction,
      spendingInfoToSatisfy: ScriptSignatureParams[InputType])
      : TxSigComponent = {
    val (_, output, inputIndex, _) =
      relevantInfo(spendingInfo, unsignedTx)

    val single = spendingInfo.toSingle(0)
    val partialSignature =
      signSingle(single, unsignedTx, single.signer.signLowRWithHashType)

    val scriptSig =
      keyAndSigToScriptSig(partialSignature.pubKey.toPublicKey,
                           partialSignature.signature,
                           spendingInfoToSatisfy)

    updateScriptSigInSigComponent(unsignedTx,
                                  inputIndex.toInt,
                                  output,
                                  scriptSig)
  }
}

/** For signing EmptyScriptPubKeys in tests, should probably not be used in real
  * life.
  */
sealed abstract class EmptySigner extends Signer[EmptyInputInfo] {

  override def sign(
      spendingInfo: ScriptSignatureParams[InputInfo],
      unsignedTx: Transaction,
      spendingInfoToSatisfy: ScriptSignatureParams[EmptyInputInfo])
      : TxSigComponent = {
    val (_, output, inputIndex, _) = relevantInfo(spendingInfo, unsignedTx)

    val satisfyEmptyScriptSig = TrivialTrueScriptSignature

    updateScriptSigInSigComponent(unsignedTx,
                                  inputIndex.toInt,
                                  output,
                                  satisfyEmptyScriptSig)
  }
}

object EmptySigner extends EmptySigner

/** Used to sign a [[org.bitcoins.core.protocol.script.P2PKScriptPubKey]] */
sealed abstract class P2PKSigner
    extends RawSingleKeyBitcoinSigner[P2PKInputInfo] {

  override def keyAndSigToScriptSig(
      key: ECPublicKey,
      sig: ECDigitalSignature,
      spendingInfo: InputSigningInfo[P2PKInputInfo]): ScriptSignature = {
    P2PKScriptSignature(sig)
  }
}

object P2PKSigner extends P2PKSigner

/** Used to sign a [[org.bitcoins.core.protocol.script.P2PKHScriptPubKey]] */
sealed abstract class P2PKHSigner
    extends RawSingleKeyBitcoinSigner[P2PKHInputInfo] {

  override def keyAndSigToScriptSig(
      key: ECPublicKey,
      sig: ECDigitalSignature,
      spendingInfo: InputSigningInfo[P2PKHInputInfo]): ScriptSignature = {
    P2PKHScriptSignature(sig, key)
  }
}

object P2PKHSigner extends P2PKHSigner

sealed abstract class P2PKWithTimeoutSigner
    extends RawSingleKeyBitcoinSigner[P2PKWithTimeoutInputInfo] {

  override def keyAndSigToScriptSig(
      key: ECPublicKey,
      sig: ECDigitalSignature,
      spendingInfo: InputSigningInfo[P2PKWithTimeoutInputInfo])
      : ScriptSignature = {
    P2PKWithTimeoutScriptSignature(spendingInfo.inputInfo.isBeforeTimeout, sig)
  }
}

object P2PKWithTimeoutSigner extends P2PKWithTimeoutSigner

sealed abstract class MultiSigSigner extends Signer[MultiSignatureInputInfo] {

  override def sign(
      spendingInfo: ScriptSignatureParams[InputInfo],
      unsignedTx: Transaction,
      spendingInfoToSatisfy: ScriptSignatureParams[MultiSignatureInputInfo])
      : TxSigComponent = {
    val (_, output, inputIndex, _) =
      relevantInfo(spendingInfo, unsignedTx)

    val keysAndSigs = spendingInfo.toSingles.map { spendingInfoSingle =>
      signSingle(spendingInfoSingle,
                 unsignedTx,
                 spendingInfoSingle.signer.signLowRWithHashType)
    }

    val signatures = keysAndSigs.map(_.signature)

    val scriptSig = MultiSignatureScriptSignature(signatures)

    updateScriptSigInSigComponent(unsignedTx,
                                  inputIndex.toInt,
                                  output,
                                  scriptSig)
  }
}

object MultiSigSigner extends MultiSigSigner

/** Used to sign a [[org.bitcoins.core.protocol.script.P2SHScriptPubKey]] */
sealed abstract class P2SHSigner extends Signer[P2SHInputInfo] {

  override def sign(
      spendingInfo: ScriptSignatureParams[InputInfo],
      unsignedTx: Transaction,
      spendingInfoToSatisfy: ScriptSignatureParams[P2SHInputInfo])
      : TxSigComponent = {
    if (spendingInfoToSatisfy != spendingInfo) {
      throw TxBuilderError.WrongSigner.exception
    } else {
      val (_, output, inputIndex, _) = relevantInfo(spendingInfo, unsignedTx)

      val oldInput = unsignedTx.inputs(inputIndex.toInt)
      val input =
        TransactionInput(spendingInfo.outPoint,
                         EmptyScriptSignature,
                         oldInput.sequence)

      val updatedTx =
        unsignedTx.updateInput(inputIndex.toInt, input)

      val nestedSpendingInfo = spendingInfoToSatisfy.copy(
        inputInfo = spendingInfoToSatisfy.inputInfo.nestedInputInfo)

      val signedTx =
        BitcoinSigner
          .sign(nestedSpendingInfo, updatedTx)
          .transaction

      val i = signedTx.inputs(inputIndex.toInt)

      val p2sh =
        P2SHScriptSignature(i.scriptSignature,
                            spendingInfoToSatisfy.inputInfo.redeemScript)

      val signedInput =
        TransactionInput(i.previousOutput, p2sh, i.sequence)

      val signedInputs =
        signedTx.inputs.updated(inputIndex.toInt, signedInput)

      val finalTx = signedTx match {
        case btx: NonWitnessTransaction =>
          BaseTransaction(version = btx.version,
                          inputs = signedInputs,
                          outputs = btx.outputs,
                          lockTime = btx.lockTime)
        case wtx: WitnessTransaction =>
          WitnessTransaction(version = wtx.version,
                             inputs = signedInputs,
                             outputs = wtx.outputs,
                             lockTime = wtx.lockTime,
                             witness = wtx.witness)
      }
      P2SHTxSigComponent(transaction = finalTx,
                         inputIndex = inputIndex,
                         output = output,
                         flags = flags)
    }
  }
}

object P2SHSigner extends P2SHSigner

sealed abstract class P2WPKHSigner extends Signer[P2WPKHV0InputInfo] {

  override def sign(
      spendingInfo: ScriptSignatureParams[InputInfo],
      unsignedTx: Transaction,
      spendingInfoToSatisfy: ScriptSignatureParams[P2WPKHV0InputInfo])
      : TxSigComponent = {
    if (spendingInfoToSatisfy != spendingInfo) {
      throw TxBuilderError.WrongSigner.exception
    } else {
      val (_, output, inputIndex, hashType) =
        relevantInfo(spendingInfo, unsignedTx)
      unsignedTx match {
        case wtx: WitnessTransaction =>
          val signer = spendingInfoToSatisfy.signer
          val pubKey = signer.publicKey

          val unsignedTxWitness = TransactionWitness(
            wtx.witness
              .updated(inputIndex.toInt,
                       spendingInfoToSatisfy.inputInfo.scriptWitness)
              .toVector)

          val unsignedWtx = WitnessTransaction(wtx.version,
                                               wtx.inputs,
                                               wtx.outputs,
                                               wtx.lockTime,
                                               unsignedTxWitness)

          val witSPK = output.scriptPubKey match {
            case p2wpkh: P2WPKHWitnessSPKV0 => p2wpkh
            case spk: TaprootScriptPubKey =>
              throw new IllegalArgumentException(
                s"Taproot not yet supported: $spk")
            case _: UnassignedWitnessScriptPubKey | _: P2WSHWitnessSPKV0 =>
              throw TxBuilderError.WrongSigner.exception
            case _: NonWitnessScriptPubKey =>
              throw TxBuilderError.NonWitnessSPK.exception
          }

          val witOutput = TransactionOutput(output.value, witSPK)

          val signature =
            doSign(unsignedTx,
                   spendingInfo,
                   signer.signLowRWithHashType,
                   hashType)

          val scriptWitness = P2WPKHWitnessV0(pubKey, signature)
          val signedTxWitness =
            wtx.witness.updated(inputIndex.toInt, scriptWitness)
          val signedTx = WitnessTransaction(unsignedWtx.version,
                                            unsignedWtx.inputs,
                                            unsignedWtx.outputs,
                                            unsignedWtx.lockTime,
                                            signedTxWitness)
          WitnessTxSigComponentRaw(signedTx, inputIndex, witOutput, flags)

        case btx: NonWitnessTransaction =>
          val wtx = WitnessTransaction.toWitnessTx(btx)

          sign(spendingInfoToSatisfy, wtx)
      }
    }
  }
}
object P2WPKHSigner extends P2WPKHSigner

sealed abstract class P2WSHSigner extends Signer[P2WSHV0InputInfo] {

  override def sign(
      spendingInfo: ScriptSignatureParams[InputInfo],
      unsignedTx: Transaction,
      spendingInfoToSatisfy: ScriptSignatureParams[P2WSHV0InputInfo])
      : TxSigComponent = {
    if (spendingInfoToSatisfy != spendingInfo) {
      throw TxBuilderError.WrongSigner.exception
    } else {
      val (_, output, inputIndex, _) = relevantInfo(spendingInfo, unsignedTx)

      val wtx = WitnessTransaction
        .toWitnessTx(unsignedTx)
        .updateWitness(inputIndex.toInt,
                       spendingInfoToSatisfy.inputInfo.scriptWitness)

      val nestedSpendingInfo = spendingInfoToSatisfy.copy(
        inputInfo = spendingInfoToSatisfy.inputInfo.nestedInputInfo)

      val signedSigComponent =
        BitcoinSigner.sign(spendingInfo, wtx, nestedSpendingInfo)

      val scriptWit =
        P2WSHWitnessV0(
          spendingInfoToSatisfy.inputInfo.scriptWitness.redeemScript,
          signedSigComponent.scriptSignature)

      val signedWitness =
        wtx.witness.updated(inputIndex.toInt, scriptWit)
      val signedWTx = WitnessTransaction(wtx.version,
                                         wtx.inputs,
                                         wtx.outputs,
                                         wtx.lockTime,
                                         signedWitness)
      WitnessTxSigComponentRaw(signedWTx, inputIndex, output, flags)
    }
  }
}
object P2WSHSigner extends P2WSHSigner

sealed abstract class LockTimeSigner extends Signer[LockTimeInputInfo] {

  override def sign(
      spendingInfo: ScriptSignatureParams[InputInfo],
      unsignedTx: Transaction,
      spendingInfoToSatisfy: ScriptSignatureParams[LockTimeInputInfo])
      : TxSigComponent = {
    val nestedSpendingInfo = spendingInfoToSatisfy.copy(
      inputInfo = spendingInfoToSatisfy.inputInfo.nestedInputInfo)

    BitcoinSigner.sign(spendingInfo, unsignedTx, nestedSpendingInfo)
  }
}
object LockTimeSigner extends LockTimeSigner

/** Delegates to get a ScriptSignature for the case being spent and then adds an
  * OP_TRUE or OP_FALSE
  */
sealed abstract class ConditionalSigner extends Signer[ConditionalInputInfo] {

  override def sign(
      spendingInfo: ScriptSignatureParams[InputInfo],
      unsignedTx: Transaction,
      spendingInfoToSatisfy: ScriptSignatureParams[ConditionalInputInfo])
      : TxSigComponent = {
    val (_, output, inputIndex, _) = relevantInfo(spendingInfo, unsignedTx)

    val nestedSpendingInfo = spendingInfoToSatisfy.copy(
      inputInfo = spendingInfoToSatisfy.inputInfo.nestedInputInfo)

    val missingOpSigComponent =
      BitcoinSigner.sign(spendingInfo, unsignedTx, nestedSpendingInfo)

    val scriptSig =
      ConditionalScriptSignature(missingOpSigComponent.scriptSignature,
                                 spendingInfoToSatisfy.inputInfo.condition)

    updateScriptSigInSigComponent(unsignedTx,
                                  inputIndex.toInt,
                                  output,
                                  scriptSig)
  }
}
object ConditionalSigner extends ConditionalSigner

sealed abstract class TaprootKeyPathSigner
    extends Signer[TaprootKeyPathInputInfo] {

  /** The method used to sign a bitcoin unspent transaction output that is
    * potentially nested
    *
    * @param spendingInfo
    *   \- The information required for signing
    * @param unsignedTx
    *   the external Transaction that needs an input signed
    * @param isDummySignature
    *   \- do not sign the tx for real, just use a dummy signature this is
    *   useful for fee estimation
    * @param spendingInfoToSatisfy
    *   \- specifies the NewSpendingInfo whose ScriptPubKey needs a
    *   ScriptSignature to be generated
    * @return
    */
  override def sign(
      spendingInfo: ScriptSignatureParams[InputInfo],
      unsignedTx: Transaction,
      spendingInfoToSatisfy: ScriptSignatureParams[TaprootKeyPathInputInfo])
      : TaprootTxSigComponent = {
    if (spendingInfoToSatisfy != spendingInfo) {
      throw TxBuilderError.WrongSigner.exception
    } else {
      unsignedTx match {
        case wtx: WitnessTransaction =>
          val signer = spendingInfoToSatisfy.signer
          val inputIndex = spendingInfoToSatisfy.inputInfo.inputIndex
          val hashType = spendingInfo.hashType
          val unsignedTxWitness = TransactionWitness(
            wtx.witness
              .updated(inputIndex,
                       spendingInfoToSatisfy.inputInfo.scriptWitness)
              .toVector)

          val unsignedWtx = wtx.copy(witness = unsignedTxWitness)

          val signature: SchnorrDigitalSignature =
            doSign(unsignedTx,
                   spendingInfo,
                   signer.schnorrSignWithHashType,
                   hashType)
          val scriptWitness = TaprootKeyPath(signature)
          val signedTxWitness =
            wtx.witness.updated(inputIndex, scriptWitness)
          val signedTx = unsignedWtx.copy(witness = signedTxWitness)

          TaprootTxSigComponent(
            transaction = signedTx,
            inputIndex = UInt32(inputIndex),
            outputMap = spendingInfoToSatisfy.inputInfo.previousOutputMap,
            flags = flags)

        case btx: NonWitnessTransaction =>
          val wtx = WitnessTransaction.toWitnessTx(btx)
          sign(spendingInfo, wtx, spendingInfoToSatisfy)
      }

    }
  }

}

object TaprootKeyPathSigner extends TaprootKeyPathSigner
