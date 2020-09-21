package org.bitcoins.core.wallet.signer

import org.bitcoins.core.crypto._
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.core.psbt.PSBT
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.script.flag.ScriptFlag
import org.bitcoins.core.wallet.builder.TxBuilderError
import org.bitcoins.core.wallet.utxo._
import org.bitcoins.crypto.{
  DummyECDigitalSignature,
  ECDigitalSignature,
  ECPublicKey,
  Sign
}
import scodec.bits.ByteVector

import scala.concurrent.{ExecutionContext, Future}

sealed abstract class SignerUtils {

  @deprecated("use an InputSigningInfo[InputInfo] instead", since = "6/23/2020")
  def doSign(
      sigComponent: TxSigComponent,
      sign: ByteVector => Future[ECDigitalSignature],
      hashType: HashType,
      isDummySignature: Boolean)(implicit
      ec: ExecutionContext): Future[ECDigitalSignature] = {
    if (isDummySignature) {
      Future.successful(DummyECDigitalSignature)
    } else {
      TransactionSignatureCreator.createSig(sigComponent, sign, hashType)
    }
  }

  def doSign(
      unsignedTx: Transaction,
      signingInfo: InputSigningInfo[InputInfo],
      sign: ByteVector => Future[ECDigitalSignature],
      hashType: HashType,
      isDummySignature: Boolean)(implicit
      ec: ExecutionContext): Future[ECDigitalSignature] = {
    if (isDummySignature) {
      Future.successful(DummyECDigitalSignature)
    } else {
      TransactionSignatureCreator.createSig(unsignedTx,
                                            signingInfo,
                                            sign,
                                            hashType)
    }
  }

  def signSingle(
      spendingInfo: ECSignatureParams[InputInfo],
      unsignedTx: Transaction,
      isDummySignature: Boolean)(implicit
      ec: ExecutionContext): Future[PartialSignature] = {

    val tx = spendingInfo.inputInfo match {
      case _: SegwitV0NativeInputInfo | _: P2SHNestedSegwitV0InputInfo |
          _: UnassignedSegwitNativeInputInfo =>
        TxUtil.addWitnessData(unsignedTx, spendingInfo)
      case _: RawInputInfo | _: P2SHNonSegwitInputInfo =>
        unsignedTx
    }

    val signatureF = doSign(
      unsignedTx = tx,
      signingInfo = spendingInfo,
      sign = spendingInfo.signer.signLowRFuture,
      hashType = spendingInfo.hashType,
      isDummySignature = isDummySignature
    )

    signatureF.map { sig =>
      PartialSignature(spendingInfo.signer.publicKey, sig)
    }
  }

  protected val flags: Seq[ScriptFlag] = Policy.standardFlags

  protected def relevantInfo(
      spendingInfo: InputSigningInfo[InputInfo],
      unsignedTx: Transaction): (
      Seq[Sign],
      TransactionOutput,
      UInt32,
      HashType) = {
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

/** The class used to represent a signing process for a specific [[org.bitcoins.core.protocol.script.ScriptPubKey]] type */
sealed abstract class Signer[-InputType <: InputInfo] extends SignerUtils {

  /**
    * The method used to sign a bitcoin unspent transaction output
    * @param spendingInfo - The information required for signing
    * @param unsignedTx the external Transaction that needs an input signed
    * @param isDummySignature - do not sign the tx for real, just use a dummy signature this is useful for fee estimation
    * @return
    */
  def sign(
      spendingInfo: ScriptSignatureParams[InputType],
      unsignedTx: Transaction,
      isDummySignature: Boolean)(implicit
      ec: ExecutionContext): Future[TxSigComponent] = {
    sign(
      spendingInfo,
      unsignedTx,
      isDummySignature,
      spendingInfoToSatisfy = spendingInfo
    )
  }

  /**
    * The method used to sign a bitcoin unspent transaction output that is potentially nested
    * @param spendingInfo - The information required for signing
    * @param unsignedTx the external Transaction that needs an input signed
    * @param isDummySignature - do not sign the tx for real, just use a dummy signature this is useful for fee estimation
    * @param spendingInfoToSatisfy - specifies the NewSpendingInfo whose ScriptPubKey needs a ScriptSignature to be generated
    * @return
    */
  def sign(
      spendingInfo: ScriptSignatureParams[InputInfo],
      unsignedTx: Transaction,
      isDummySignature: Boolean,
      spendingInfoToSatisfy: ScriptSignatureParams[InputType])(implicit
      ec: ExecutionContext): Future[TxSigComponent]

  /** Creates a BaseTxSigComponent by replacing the unsignedTx input at inputIndex
    * with a signed one using the given ScriptSignature
    */
  protected def updateScriptSigInSigComponent(
      unsignedTx: Transaction,
      inputIndex: Int,
      output: TransactionOutput,
      scriptSignatureF: Future[ScriptSignature])(implicit
      ec: ExecutionContext): Future[BaseTxSigComponent] = {
    val unsignedInput = unsignedTx.inputs(inputIndex)

    scriptSignatureF.map { signature =>
      val signedInput = TransactionInput(unsignedInput.previousOutput,
                                         signature,
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
}

object BitcoinSigner extends SignerUtils {

  def sign(
      spendingInfo: ScriptSignatureParams[InputInfo],
      unsignedTx: Transaction,
      isDummySignature: Boolean)(implicit
      ec: ExecutionContext): Future[TxSigComponent] = {
    sign(spendingInfo, unsignedTx, isDummySignature, spendingInfo)
  }

  def sign(
      spendingInfo: ScriptSignatureParams[InputInfo],
      unsignedTx: Transaction,
      isDummySignature: Boolean,
      spendingInfoToSatisfy: ScriptSignatureParams[InputInfo])(implicit
      ec: ExecutionContext): Future[TxSigComponent] = {
    def spendingFrom[Info <: InputInfo](
        inputInfo: Info): ScriptSignatureParams[Info] = {
      spendingInfoToSatisfy.copy(inputInfo = inputInfo)
    }

    spendingInfoToSatisfy.inputInfo match {
      case empty: EmptyInputInfo =>
        EmptySigner.sign(spendingInfo,
                         unsignedTx,
                         isDummySignature,
                         spendingFrom(empty))
      case p2pk: P2PKInputInfo =>
        P2PKSigner.sign(spendingInfo,
                        unsignedTx,
                        isDummySignature,
                        spendingFrom(p2pk))
      case p2pkh: P2PKHInputInfo =>
        P2PKHSigner.sign(spendingInfo,
                         unsignedTx,
                         isDummySignature,
                         spendingFrom(p2pkh))
      case p2pKWithTimeout: P2PKWithTimeoutInputInfo =>
        P2PKWithTimeoutSigner.sign(spendingInfo,
                                   unsignedTx,
                                   isDummySignature,
                                   spendingFrom(p2pKWithTimeout))
      case p2sh: P2SHInputInfo =>
        P2SHSigner.sign(spendingInfo,
                        unsignedTx,
                        isDummySignature,
                        spendingFrom(p2sh))
      case multiSig: MultiSignatureInputInfo =>
        MultiSigSigner.sign(spendingInfo,
                            unsignedTx,
                            isDummySignature,
                            spendingFrom(multiSig))
      case lockTime: LockTimeInputInfo =>
        LockTimeSigner.sign(spendingInfo,
                            unsignedTx,
                            isDummySignature,
                            spendingFrom(lockTime))
      case conditional: ConditionalInputInfo =>
        ConditionalSigner.sign(spendingInfo,
                               unsignedTx,
                               isDummySignature,
                               spendingFrom(conditional))
      case p2wpkh: P2WPKHV0InputInfo =>
        P2WPKHSigner.sign(spendingInfo,
                          unsignedTx,
                          isDummySignature,
                          spendingFrom(p2wpkh))
      case pw2sh: P2WSHV0InputInfo =>
        P2WSHSigner.sign(spendingInfo,
                         unsignedTx,
                         isDummySignature,
                         spendingFrom(pw2sh))
      case _: UnassignedSegwitNativeInputInfo =>
        throw new UnsupportedOperationException("Unsupported Segwit version")
    }
  }

  /**
    * Signs the PSBT's input at the given input with the signer, then adds it to the PSBT
    * in a PartialSignature record
    * @param psbt The PSBT to sign
    * @param inputIndex Index of input to sign
    * @param signer Function or private key used to sign the PSBT
    * @param isDummySignature Do not sign the tx for real, just use a dummy signature, this is useful for fee estimation
    * @param conditionalPath Represents the spending branch being taken in a ScriptPubKey's execution
    * @return
    */
  def sign(
      psbt: PSBT,
      inputIndex: Int,
      signer: Sign,
      conditionalPath: ConditionalPath = ConditionalPath.NoCondition,
      isDummySignature: Boolean = false)(implicit
      ec: ExecutionContext): Future[PSBT] = {
    // if already signed by this signer
    if (
      psbt
        .inputMaps(inputIndex)
        .partialSignatures
        .exists(_.pubKey == signer.publicKey)
    ) {
      Future.failed(
        new IllegalArgumentException(
          "Input has already been signed with this key"))
    }

    val tx = psbt.transaction
    val spendingInfo =
      psbt
        .inputMaps(inputIndex)
        .toUTXOSigningInfo(tx.inputs(inputIndex), signer, conditionalPath)

    val txToSign = spendingInfo.output.scriptPubKey match {
      case _: WitnessScriptPubKey =>
        tx match {
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

              case _: P2WPKHWitnessV0 | _: P2WSHWitnessV0 => wtx
            }
        }
      case _: ScriptPubKey => tx
    }

    val partialSignatureF =
      signSingle(spendingInfo, txToSign, isDummySignature)

    partialSignatureF.map { partialSignature =>
      psbt.addSignature(partialSignature, inputIndex)
    }
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
      isDummySignature: Boolean,
      spendingInfoToSatisfy: ScriptSignatureParams[InputType])(implicit
      ec: ExecutionContext): Future[TxSigComponent] = {
    val (_, output, inputIndex, _) =
      relevantInfo(spendingInfo, unsignedTx)

    val partialSignatureF =
      signSingle(spendingInfo.toSingle(0), unsignedTx, isDummySignature)

    val scriptSigF = partialSignatureF.map { partialSignature =>
      keyAndSigToScriptSig(partialSignature.pubKey,
                           partialSignature.signature,
                           spendingInfoToSatisfy)
    }

    updateScriptSigInSigComponent(unsignedTx,
                                  inputIndex.toInt,
                                  output,
                                  scriptSigF)
  }
}

/** For signing EmptyScriptPubKeys in tests, should probably not be used in real life. */
sealed abstract class EmptySigner extends Signer[EmptyInputInfo] {

  override def sign(
      spendingInfo: ScriptSignatureParams[InputInfo],
      unsignedTx: Transaction,
      isDummySignature: Boolean,
      spendingInfoToSatisfy: ScriptSignatureParams[EmptyInputInfo])(implicit
      ec: ExecutionContext): Future[TxSigComponent] = {
    val (_, output, inputIndex, _) = relevantInfo(spendingInfo, unsignedTx)

    val satisfyEmptyScriptSig =
      Future.successful(TrivialTrueScriptSignature)

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
      spendingInfo: InputSigningInfo[
        P2PKWithTimeoutInputInfo]): ScriptSignature = {
    P2PKWithTimeoutScriptSignature(spendingInfo.inputInfo.isBeforeTimeout, sig)
  }
}

object P2PKWithTimeoutSigner extends P2PKWithTimeoutSigner

sealed abstract class MultiSigSigner extends Signer[MultiSignatureInputInfo] {

  override def sign(
      spendingInfo: ScriptSignatureParams[InputInfo],
      unsignedTx: Transaction,
      isDummySignature: Boolean,
      spendingInfoToSatisfy: ScriptSignatureParams[MultiSignatureInputInfo])(
      implicit ec: ExecutionContext): Future[TxSigComponent] = {
    val (_, output, inputIndex, _) =
      relevantInfo(spendingInfo, unsignedTx)

    val keysAndSigsF = spendingInfo.toSingles.map { spendingInfoSingle =>
      signSingle(spendingInfoSingle, unsignedTx, isDummySignature)
    }

    val signaturesF = Future.sequence(keysAndSigsF).map(_.map(_.signature))

    val scriptSigF = signaturesF.map { sigs =>
      MultiSignatureScriptSignature(sigs)
    }

    updateScriptSigInSigComponent(unsignedTx,
                                  inputIndex.toInt,
                                  output,
                                  scriptSigF)
  }
}

object MultiSigSigner extends MultiSigSigner

/** Used to sign a [[org.bitcoins.core.protocol.script.P2SHScriptPubKey]] */
sealed abstract class P2SHSigner extends Signer[P2SHInputInfo] {

  override def sign(
      spendingInfo: ScriptSignatureParams[InputInfo],
      unsignedTx: Transaction,
      isDummySignature: Boolean,
      spendingInfoToSatisfy: ScriptSignatureParams[P2SHInputInfo])(implicit
      ec: ExecutionContext): Future[TxSigComponent] = {
    if (spendingInfoToSatisfy != spendingInfo) {
      Future.fromTry(TxBuilderError.WrongSigner)
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

      val signedTxEither =
        BitcoinSigner
          .sign(nestedSpendingInfo, updatedTx, isDummySignature)
          .map(_.transaction)

      signedTxEither.map { signedTx =>
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
}

object P2SHSigner extends P2SHSigner

sealed abstract class P2WPKHSigner extends Signer[P2WPKHV0InputInfo] {

  override def sign(
      spendingInfo: ScriptSignatureParams[InputInfo],
      unsignedTx: Transaction,
      isDummySignature: Boolean,
      spendingInfoToSatisfy: ScriptSignatureParams[P2WPKHV0InputInfo])(implicit
      ec: ExecutionContext): Future[TxSigComponent] = {
    if (spendingInfoToSatisfy != spendingInfo) {
      Future.fromTry(TxBuilderError.WrongSigner)
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
            case p2wpkh: P2WPKHWitnessSPKV0 => Future.successful(p2wpkh)
            case _: UnassignedWitnessScriptPubKey | _: P2WSHWitnessSPKV0 =>
              Future.fromTry(TxBuilderError.WrongSigner)
            case _: NonWitnessScriptPubKey =>
              Future.fromTry(TxBuilderError.NonWitnessSPK)
          }

          witSPK.flatMap { w =>
            val witOutput = TransactionOutput(output.value, w)

            val signature =
              doSign(unsignedTx,
                     spendingInfo,
                     signer.signLowRFuture,
                     hashType,
                     isDummySignature)

            signature.map { sig =>
              val scriptWitness = P2WPKHWitnessV0(pubKey, sig)
              val signedTxWitness =
                wtx.witness.updated(inputIndex.toInt, scriptWitness)
              val signedTx = WitnessTransaction(unsignedWtx.version,
                                                unsignedWtx.inputs,
                                                unsignedWtx.outputs,
                                                unsignedWtx.lockTime,
                                                signedTxWitness)
              WitnessTxSigComponentRaw(signedTx, inputIndex, witOutput, flags)
            }

          }
        case btx: NonWitnessTransaction =>
          val wtx = WitnessTransaction.toWitnessTx(btx)

          sign(spendingInfoToSatisfy, wtx, isDummySignature)
      }
    }
  }
}
object P2WPKHSigner extends P2WPKHSigner

sealed abstract class P2WSHSigner extends Signer[P2WSHV0InputInfo] {

  override def sign(
      spendingInfo: ScriptSignatureParams[InputInfo],
      unsignedTx: Transaction,
      isDummySignature: Boolean,
      spendingInfoToSatisfy: ScriptSignatureParams[P2WSHV0InputInfo])(implicit
      ec: ExecutionContext): Future[TxSigComponent] = {
    if (spendingInfoToSatisfy != spendingInfo) {
      Future.fromTry(TxBuilderError.WrongSigner)
    } else {
      val (_, output, inputIndex, _) = relevantInfo(spendingInfo, unsignedTx)

      val wtx = WitnessTransaction
        .toWitnessTx(unsignedTx)
        .updateWitness(inputIndex.toInt,
                       spendingInfoToSatisfy.inputInfo.scriptWitness)

      val nestedSpendingInfo = spendingInfoToSatisfy.copy(
        inputInfo = spendingInfoToSatisfy.inputInfo.nestedInputInfo)

      val signedSigComponentF = BitcoinSigner.sign(spendingInfo,
                                                   wtx,
                                                   isDummySignature,
                                                   nestedSpendingInfo)

      val scriptWitF = signedSigComponentF.map { signedSigComponent =>
        P2WSHWitnessV0(
          spendingInfoToSatisfy.inputInfo.scriptWitness.redeemScript,
          signedSigComponent.scriptSignature)
      }

      scriptWitF.map { scriptWit =>
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
}
object P2WSHSigner extends P2WSHSigner

sealed abstract class LockTimeSigner extends Signer[LockTimeInputInfo] {

  override def sign(
      spendingInfo: ScriptSignatureParams[InputInfo],
      unsignedTx: Transaction,
      isDummySignature: Boolean,
      spendingInfoToSatisfy: ScriptSignatureParams[LockTimeInputInfo])(implicit
      ec: ExecutionContext): Future[TxSigComponent] = {
    val nestedSpendingInfo = spendingInfoToSatisfy.copy(
      inputInfo = spendingInfoToSatisfy.inputInfo.nestedInputInfo)

    BitcoinSigner.sign(spendingInfo,
                       unsignedTx,
                       isDummySignature,
                       nestedSpendingInfo)
  }
}
object LockTimeSigner extends LockTimeSigner

/** Delegates to get a ScriptSignature for the case being
  * spent and then adds an OP_TRUE or OP_FALSE
  */
sealed abstract class ConditionalSigner extends Signer[ConditionalInputInfo] {

  override def sign(
      spendingInfo: ScriptSignatureParams[InputInfo],
      unsignedTx: Transaction,
      isDummySignature: Boolean,
      spendingInfoToSatisfy: ScriptSignatureParams[ConditionalInputInfo])(
      implicit ec: ExecutionContext): Future[TxSigComponent] = {
    val (_, output, inputIndex, _) = relevantInfo(spendingInfo, unsignedTx)

    val nestedSpendingInfo = spendingInfoToSatisfy.copy(
      inputInfo = spendingInfoToSatisfy.inputInfo.nestedInputInfo)

    val missingOpSigComponentF = BitcoinSigner.sign(spendingInfo,
                                                    unsignedTx,
                                                    isDummySignature,
                                                    nestedSpendingInfo)

    val scriptSigF = missingOpSigComponentF.map { sigComponent =>
      ConditionalScriptSignature(sigComponent.scriptSignature,
                                 spendingInfoToSatisfy.inputInfo.condition)
    }

    updateScriptSigInSigComponent(unsignedTx,
                                  inputIndex.toInt,
                                  output,
                                  scriptSigF)
  }
}
object ConditionalSigner extends ConditionalSigner
