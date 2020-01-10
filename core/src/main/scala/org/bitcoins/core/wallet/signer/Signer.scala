package org.bitcoins.core.wallet.signer

import org.bitcoins.core.crypto._
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.script.flag.ScriptFlag
import org.bitcoins.core.wallet.builder.TxBuilderError
import org.bitcoins.core.wallet.utxo._
import scodec.bits.ByteVector

import scala.concurrent.{ExecutionContext, Future}

abstract private[signer] class SignerUtils {

  def doSign(
      sigComponent: TxSigComponent,
      sign: ByteVector => Future[ECDigitalSignature],
      hashType: HashType,
      isDummySignature: Boolean)(
      implicit ec: ExecutionContext): Future[ECDigitalSignature] = {
    if (isDummySignature) {
      Future.successful(DummyECDigitalSignature)
    } else {
      TransactionSignatureCreator.createSig(sigComponent, sign, hashType)
    }
  }

  protected val flags: Seq[ScriptFlag] = Policy.standardFlags

  protected def relevantInfo(
      spendingInfo: UTXOSpendingInfoSingle,
      unsignedTx: Transaction): (Seq[Sign], TransactionOutput, UInt32, HashType) = {
    val signers = spendingInfo match {
      case info: UTXOSpendingInfo    => info.signers
      case _: UTXOSpendingInfoSingle => Vector(spendingInfo.signer)
    }

    (signers,
     spendingInfo.output,
     inputIndex(spendingInfo, unsignedTx),
     spendingInfo.hashType)
  }

  protected def inputIndex(
      spendingInfo: UTXOSpendingInfoSingle,
      tx: Transaction): UInt32 = {
    tx.inputs.zipWithIndex
      .find(_._1.previousOutput == spendingInfo.outPoint) match {
      case Some((_, index)) => UInt32(index)
      case None =>
        throw new IllegalArgumentException(
          "Transaction did not contain expected input.")
    }
  }

  protected def sigComponent(
      spendingInfo: UTXOSpendingInfoSingle,
      unsignedTx: Transaction): TxSigComponent = {
    val index = inputIndex(spendingInfo, unsignedTx)

    spendingInfo.output.scriptPubKey match {
      case _: WitnessScriptPubKey =>
        val wtx = unsignedTx match {
          case btx: BaseTransaction =>
            val transactionWitnessOpt =
              spendingInfo.scriptWitnessOpt.map(scriptWit =>
                TransactionWitness(Vector(scriptWit)))
            val transactionWitness =
              transactionWitnessOpt.getOrElse(
                EmptyWitness.fromInputs(btx.inputs))

            WitnessTransaction(btx.version,
                               btx.inputs,
                               btx.outputs,
                               btx.lockTime,
                               transactionWitness)
          case wtx: WitnessTransaction => wtx
        }

        WitnessTxSigComponent(wtx, index, spendingInfo.output, flags)
      case _: P2SHScriptPubKey =>
        P2SHTxSigComponent(unsignedTx, index, spendingInfo.output, flags)
      case _: ScriptPubKey =>
        BaseTxSigComponent(unsignedTx, index, spendingInfo.output, flags)
    }
  }
}

sealed trait SingleSigner[-SpendingInfo <: UTXOSpendingInfoSingle]
    extends SignerUtils {

  def signSingle(
      spendingInfo: SpendingInfo,
      unsignedTx: Transaction,
      isDummySignature: Boolean)(implicit ec: ExecutionContext): Future[
    (ECPublicKey, ECDigitalSignature)] = {
    signSingle(spendingInfo,
               unsignedTx,
               isDummySignature,
               spendingInfoToSatisfy = spendingInfo)
  }

  def signSingle(
      spendingInfo: UTXOSpendingInfoSingle,
      unsignedTx: Transaction,
      isDummySignature: Boolean,
      spendingInfoToSatisfy: SpendingInfo)(
      implicit ec: ExecutionContext): Future[(ECPublicKey, ECDigitalSignature)]
}

/** The class used to represent a signing process for a specific [[org.bitcoins.core.protocol.script.ScriptPubKey]] type */
sealed abstract class Signer[-SpendingInfo <: UTXOSpendingInfo]
    extends SignerUtils {

  /**
    * The method used to sign a bitcoin unspent transaction output
    * @param spendingInfo - The information required for signing
    * @param unsignedTx the external Transaction that needs an input signed
    * @param isDummySignature - do not sign the tx for real, just use a dummy signature this is useful for fee estimation
    * @return
    */
  def sign(
      spendingInfo: SpendingInfo,
      unsignedTx: Transaction,
      isDummySignature: Boolean)(
      implicit ec: ExecutionContext): Future[TxSigComponent] = {
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
    * @param spendingInfoToSatisfy - specifies the UTXOSpendingInfo whose ScriptPubKey needs a ScriptSignature to be generated
    * @return
    */
  def sign(
      spendingInfo: UTXOSpendingInfo,
      unsignedTx: Transaction,
      isDummySignature: Boolean,
      spendingInfoToSatisfy: SpendingInfo)(
      implicit ec: ExecutionContext): Future[TxSigComponent]

  /** Creates a BaseTxSigComponent by replacing the unsignedTx input at inputIndex
    * with a signed one using the given ScriptSignature
    */
  protected def updateScriptSigInSigComponent(
      unsignedTx: Transaction,
      inputIndex: Int,
      output: TransactionOutput,
      scriptSignatureF: Future[ScriptSignature])(
      implicit ec: ExecutionContext): Future[BaseTxSigComponent] = {
    val unsignedInput = unsignedTx.inputs(inputIndex)

    scriptSignatureF.map { signature =>
      val signedInput = TransactionInput(unsignedInput.previousOutput,
                                         signature,
                                         unsignedInput.sequence)
      val signedInputs = unsignedTx.inputs.updated(inputIndex, signedInput)
      val signedTx = unsignedTx match {
        case btx: BaseTransaction =>
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

sealed trait BitcoinSignerSingle[-SpendingInfo <: BitcoinUTXOSpendingInfoSingle]
    extends SingleSigner[SpendingInfo] {
  override def signSingle(
      spendingInfo: UTXOSpendingInfoSingle,
      unsignedTx: Transaction,
      isDummySignature: Boolean,
      spendingInfoToSatisfy: SpendingInfo)(
      implicit ec: ExecutionContext): Future[
    (ECPublicKey, ECDigitalSignature)] = {
    val signatureF = doSign(
      sigComponent = sigComponent(spendingInfo, unsignedTx),
      sign = spendingInfoToSatisfy.signer.signFunction,
      hashType = spendingInfoToSatisfy.hashType,
      isDummySignature = isDummySignature
    )

    signatureF.map { sig =>
      (spendingInfoToSatisfy.signer.publicKey, sig)
    }
  }
}

object BitcoinSignerSingle {

  def signSingle(
      spendingInfo: UTXOSpendingInfoSingle,
      unsignedTx: Transaction,
      isDummySignature: Boolean)(implicit ec: ExecutionContext): Future[
    (ECPublicKey, ECDigitalSignature)] = {
    signSingle(spendingInfo, unsignedTx, isDummySignature, spendingInfo)
  }

  def signSingle(
      spendingInfo: UTXOSpendingInfoSingle,
      unsignedTx: Transaction,
      isDummySignature: Boolean,
      spendingInfoToSatisfy: UTXOSpendingInfoSingle)(
      implicit ec: ExecutionContext): Future[
    (ECPublicKey, ECDigitalSignature)] = {
    spendingInfoToSatisfy match {
      case p2pk: P2PKSpendingInfo =>
        P2PKSigner.signSingle(spendingInfo, unsignedTx, isDummySignature, p2pk)
      case p2pkh: P2PKHSpendingInfo =>
        P2PKHSigner.signSingle(spendingInfo,
                               unsignedTx,
                               isDummySignature,
                               p2pkh)
      case p2pkWithTimeout: P2PKWithTimeoutSpendingInfo =>
        P2PKWithTimeoutSigner.signSingle(spendingInfo,
                                         unsignedTx,
                                         isDummySignature,
                                         p2pkWithTimeout)
      case multiSig: MultiSignatureSpendingInfoSingle =>
        MultiSigSignerSingle.signSingle(spendingInfo,
                                        unsignedTx,
                                        isDummySignature,
                                        multiSig)
      case p2sh: P2SHSpendingInfoSingle =>
        P2SHSignerSingle.signSingle(spendingInfo,
                                    unsignedTx,
                                    isDummySignature,
                                    p2sh)
      case lockTime: LockTimeSpendingInfoSingle =>
        LockTimeSignerSingle.signSingle(spendingInfo,
                                        unsignedTx,
                                        isDummySignature,
                                        lockTime)
      case conditional: ConditionalSpendingInfoSingle =>
        ConditionalSignerSingle.signSingle(spendingInfo,
                                           unsignedTx,
                                           isDummySignature,
                                           conditional)
      case p2wpkh: P2WPKHV0SpendingInfo =>
        P2WPKHSigner.signSingle(spendingInfo,
                                unsignedTx,
                                isDummySignature,
                                p2wpkh)
      case p2wsh: P2WSHV0SpendingInfoSingle =>
        P2WSHSignerSingle.signSingle(spendingInfo,
                                     unsignedTx,
                                     isDummySignature,
                                     p2wsh)
      case _: UnassignedSegwitNativeUTXOSpendingInfo =>
        throw new UnsupportedOperationException("Unsupported Segwit version")
      case _: MultiSignatureSpendingInfoFull | _: P2SHSpendingInfo |
          _: LockTimeSpendingInfoFull | _: ConditionalSpendingInfoFull |
          _: P2WSHV0SpendingInfoFull | _: EmptySpendingInfo =>
        throw new IllegalArgumentException(
          s"You should not be using signSingle with $spendingInfoToSatisfy. If you really just want one signature, call toSingle first.")
    }
  }
}

/** Represents all signers for the bitcoin protocol, we could add another network later like litecoin */
sealed abstract class BitcoinSigner[-SpendingInfo <: BitcoinUTXOSpendingInfo]
    extends Signer[SpendingInfo]

object BitcoinSigner {

  def sign(
      spendingInfo: UTXOSpendingInfo,
      unsignedTx: Transaction,
      isDummySignature: Boolean)(
      implicit ec: ExecutionContext): Future[TxSigComponent] = {
    sign(spendingInfo, unsignedTx, isDummySignature, spendingInfo)
  }

  def sign(
      spendingInfo: UTXOSpendingInfo,
      unsignedTx: Transaction,
      isDummySignature: Boolean,
      spendingInfoToSatisfy: UTXOSpendingInfo)(
      implicit ec: ExecutionContext): Future[TxSigComponent] = {
    spendingInfoToSatisfy match {
      case empty: EmptySpendingInfo =>
        EmptySigner.sign(spendingInfo, unsignedTx, isDummySignature, empty)
      case p2pk: P2PKSpendingInfo =>
        P2PKSigner.sign(spendingInfo, unsignedTx, isDummySignature, p2pk)
      case p2pkh: P2PKHSpendingInfo =>
        P2PKHSigner.sign(spendingInfo, unsignedTx, isDummySignature, p2pkh)
      case p2pKWithTimeout: P2PKWithTimeoutSpendingInfo =>
        P2PKWithTimeoutSigner.sign(spendingInfo,
                                   unsignedTx,
                                   isDummySignature,
                                   p2pKWithTimeout)
      case p2sh: P2SHSpendingInfo =>
        P2SHSigner.sign(spendingInfo, unsignedTx, isDummySignature, p2sh)
      case multiSig: MultiSignatureSpendingInfoFull =>
        MultiSigSigner.sign(spendingInfo,
                            unsignedTx,
                            isDummySignature,
                            multiSig)
      case lockTime: LockTimeSpendingInfoFull =>
        LockTimeSigner.sign(spendingInfo,
                            unsignedTx,
                            isDummySignature,
                            lockTime)
      case conditional: ConditionalSpendingInfoFull =>
        ConditionalSigner.sign(spendingInfo,
                               unsignedTx,
                               isDummySignature,
                               conditional)
      case p2wpkh: P2WPKHV0SpendingInfo =>
        P2WPKHSigner.sign(spendingInfo, unsignedTx, isDummySignature, p2wpkh)
      case pw2sh: P2WSHV0SpendingInfoFull =>
        P2WSHSigner.sign(spendingInfo, unsignedTx, isDummySignature, pw2sh)
      case _: UnassignedSegwitNativeUTXOSpendingInfo =>
        throw new UnsupportedOperationException("Unsupported Segwit version")
    }
  }
}

sealed abstract class SingleKeyBitcoinSigner[
    -SpendingInfo <: BitcoinUTXOSpendingInfo]
    extends BitcoinSigner[SpendingInfo]
    with BitcoinSignerSingle[SpendingInfo]

sealed abstract class RawSingleKeyBitcoinSigner[
    -SpendingInfo <: RawScriptUTXOSpendingInfo]
    extends SingleKeyBitcoinSigner[SpendingInfo] {

  def keyAndSigToScriptSig(
      key: ECPublicKey,
      sig: ECDigitalSignature,
      spendingInfo: SpendingInfo): ScriptSignature

  override def sign(
      spendingInfo: UTXOSpendingInfo,
      unsignedTx: Transaction,
      isDummySignature: Boolean,
      spendingInfoToSatisfy: SpendingInfo)(
      implicit ec: ExecutionContext): Future[TxSigComponent] = {
    val (_, output, inputIndex, _) =
      relevantInfo(spendingInfo, unsignedTx)

    val keyAndSigF = signSingle(spendingInfo,
                                unsignedTx,
                                isDummySignature,
                                spendingInfoToSatisfy)

    val scriptSigF = keyAndSigF.map {
      case (key, sig) =>
        keyAndSigToScriptSig(key, sig, spendingInfoToSatisfy)
    }

    updateScriptSigInSigComponent(unsignedTx,
                                  inputIndex.toInt,
                                  output,
                                  scriptSigF)
  }
}

/** For signing EmptyScriptPubKeys in tests, should probably not be used in real life. */
sealed abstract class EmptySigner extends BitcoinSigner[EmptySpendingInfo] {

  override def sign(
      spendingInfo: UTXOSpendingInfo,
      unsignedTx: Transaction,
      isDummySignature: Boolean,
      spendingInfoToSatisfy: EmptySpendingInfo)(
      implicit ec: ExecutionContext): Future[TxSigComponent] = {
    val (_, output, inputIndex, _) = relevantInfo(spendingInfo, unsignedTx)

    // This script pushes an OP_TRUE onto the stack, causing a successful spend
    val satisfyEmptyScriptSig =
      Future.successful(NonStandardScriptSignature("0151"))

    updateScriptSigInSigComponent(unsignedTx,
                                  inputIndex.toInt,
                                  output,
                                  satisfyEmptyScriptSig)
  }
}

object EmptySigner extends EmptySigner

/** Used to sign a [[org.bitcoins.core.protocol.script.P2PKScriptPubKey]] */
sealed abstract class P2PKSigner
    extends RawSingleKeyBitcoinSigner[P2PKSpendingInfo] {

  override def keyAndSigToScriptSig(
      key: ECPublicKey,
      sig: ECDigitalSignature,
      spendingInfo: P2PKSpendingInfo): ScriptSignature = {
    P2PKScriptSignature(sig)
  }
}

object P2PKSigner extends P2PKSigner

/** Used to sign a [[org.bitcoins.core.protocol.script.P2PKHScriptPubKey]] */
sealed abstract class P2PKHSigner
    extends RawSingleKeyBitcoinSigner[P2PKHSpendingInfo] {

  override def keyAndSigToScriptSig(
      key: ECPublicKey,
      sig: ECDigitalSignature,
      spendingInfo: P2PKHSpendingInfo): ScriptSignature = {
    P2PKHScriptSignature(sig, key)
  }
}

object P2PKHSigner extends P2PKHSigner

sealed abstract class P2PKWithTimeoutSigner
    extends RawSingleKeyBitcoinSigner[P2PKWithTimeoutSpendingInfo] {

  override def keyAndSigToScriptSig(
      key: ECPublicKey,
      sig: ECDigitalSignature,
      spendingInfo: P2PKWithTimeoutSpendingInfo): ScriptSignature = {
    P2PKWithTimeoutScriptSignature(spendingInfo.isBeforeTimeout, sig)
  }
}

object P2PKWithTimeoutSigner extends P2PKWithTimeoutSigner

sealed abstract class MultiSigSignerSingle
    extends BitcoinSignerSingle[MultiSignatureSpendingInfoSingle]

object MultiSigSignerSingle extends MultiSigSignerSingle

sealed abstract class MultiSigSigner
    extends BitcoinSigner[MultiSignatureSpendingInfoFull] {

  override def sign(
      spendingInfo: UTXOSpendingInfo,
      unsignedTx: Transaction,
      isDummySignature: Boolean,
      spendingInfoToSatisfy: MultiSignatureSpendingInfoFull)(
      implicit ec: ExecutionContext): Future[TxSigComponent] = {
    val (_, output, inputIndex, _) =
      relevantInfo(spendingInfo, unsignedTx)

    val requiredSigs = spendingInfoToSatisfy.requiredSigs
    val keysAndSigsF = spendingInfoToSatisfy.toSingles.take(requiredSigs).map {
      infoSingle =>
        MultiSigSignerSingle
          .signSingle(spendingInfo, unsignedTx, isDummySignature, infoSingle)
    }

    val signaturesF = Future.sequence(keysAndSigsF).map(_.map(_._2))

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

sealed abstract class P2SHSignerSingle
    extends BitcoinSignerSingle[P2SHSpendingInfoSingle] {

  override def signSingle(
      spendingInfo: UTXOSpendingInfoSingle,
      unsignedTx: Transaction,
      isDummySignature: Boolean,
      spendingInfoToSatisfy: P2SHSpendingInfoSingle)(
      implicit ec: ExecutionContext): Future[
    (ECPublicKey, ECDigitalSignature)] = {
    if (spendingInfoToSatisfy != spendingInfo) {
      Future.fromTry(TxBuilderError.WrongSigner)
    } else {
      val inputIndex = super.inputIndex(spendingInfo, unsignedTx)

      val oldInput = unsignedTx.inputs(inputIndex.toInt)
      val input =
        TransactionInput(spendingInfo.outPoint,
                         EmptyScriptSignature,
                         oldInput.sequence)

      val updatedTx =
        unsignedTx.updateInput(inputIndex.toInt, input)

      BitcoinSignerSingle
        .signSingle(spendingInfoToSatisfy.nestedSpendingInfo,
                    updatedTx,
                    isDummySignature)
    }
  }
}

object P2SHSignerSingle extends P2SHSignerSingle

/** Used to sign a [[org.bitcoins.core.protocol.script.P2SHScriptPubKey]] */
sealed abstract class P2SHSigner extends BitcoinSigner[P2SHSpendingInfo] {
  override def sign(
      spendingInfo: UTXOSpendingInfo,
      unsignedTx: Transaction,
      isDummySignature: Boolean,
      spendingInfoToSatisfy: P2SHSpendingInfo)(
      implicit ec: ExecutionContext): Future[TxSigComponent] = {
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

      val signedTxEither =
        BitcoinSigner
          .sign(spendingInfoToSatisfy.nestedSpendingInfo,
                updatedTx,
                isDummySignature)
          .map(_.transaction)

      signedTxEither.map { signedTx =>
        val i = signedTx.inputs(inputIndex.toInt)

        val p2sh =
          P2SHScriptSignature(i.scriptSignature,
                              spendingInfoToSatisfy.redeemScript)

        val signedInput =
          TransactionInput(i.previousOutput, p2sh, i.sequence)

        val signedInputs =
          signedTx.inputs.updated(inputIndex.toInt, signedInput)

        val finalTx = signedTx match {
          case btx: BaseTransaction =>
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

sealed abstract class P2WPKHSigner
    extends SingleKeyBitcoinSigner[P2WPKHV0SpendingInfo] {

  override def signSingle(
      spendingInfo: UTXOSpendingInfoSingle,
      unsignedTx: Transaction,
      isDummySignature: Boolean,
      spendingInfoToSatisfy: P2WPKHV0SpendingInfo)(
      implicit ec: ExecutionContext): Future[
    (ECPublicKey, ECDigitalSignature)] = {
    if (spendingInfoToSatisfy != spendingInfo) {
      Future.fromTry(TxBuilderError.WrongSigner)
    } else {
      for {
        sigComponent <- sign(spendingInfoToSatisfy,
                             unsignedTx,
                             isDummySignature,
                             spendingInfoToSatisfy)
        sig = sigComponent
          .asInstanceOf[WitnessTxSigComponent]
          .witness
          .asInstanceOf[script.P2WPKHWitnessV0]
          .signature
      } yield {
        (spendingInfoToSatisfy.signer.publicKey, sig)
      }
    }
  }

  override def sign(
      spendingInfo: UTXOSpendingInfo,
      unsignedTx: Transaction,
      isDummySignature: Boolean,
      spendingInfoToSatisfy: P2WPKHV0SpendingInfo)(
      implicit ec: ExecutionContext): Future[TxSigComponent] = {
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
            wtx.witness.witnesses
              .updated(inputIndex.toInt, spendingInfoToSatisfy.scriptWitness))

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

            val wtxComp = WitnessTxSigComponentRaw(unsignedWtx,
                                                   inputIndex,
                                                   witOutput,
                                                   flags)

            val signature =
              doSign(wtxComp, signer.signFunction, hashType, isDummySignature)

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
        case btx: BaseTransaction =>
          val wtx = WitnessTransaction.toWitnessTx(btx)

          sign(spendingInfoToSatisfy, wtx, isDummySignature)
      }
    }
  }
}
object P2WPKHSigner extends P2WPKHSigner

sealed abstract class P2WSHSignerSingle
    extends BitcoinSignerSingle[P2WSHV0SpendingInfoSingle] {
  override def signSingle(
      spendingInfo: UTXOSpendingInfoSingle,
      unsignedTx: Transaction,
      isDummySignature: Boolean,
      spendingInfoToSatisfy: P2WSHV0SpendingInfoSingle)(
      implicit ec: ExecutionContext): Future[
    (ECPublicKey, ECDigitalSignature)] = {
    if (spendingInfoToSatisfy != spendingInfo) {
      Future.fromTry(TxBuilderError.WrongSigner)
    } else {
      val wtx = WitnessTransaction.toWitnessTx(unsignedTx)

      BitcoinSignerSingle.signSingle(spendingInfo,
                                     wtx,
                                     isDummySignature,
                                     spendingInfoToSatisfy.nestedSpendingInfo)
    }
  }
}

object P2WSHSignerSingle extends P2WSHSignerSingle

sealed abstract class P2WSHSigner
    extends BitcoinSigner[P2WSHV0SpendingInfoFull] {

  override def sign(
      spendingInfo: UTXOSpendingInfo,
      unsignedTx: Transaction,
      isDummySignature: Boolean,
      spendingInfoToSatisfy: P2WSHV0SpendingInfoFull)(
      implicit ec: ExecutionContext): Future[TxSigComponent] = {
    if (spendingInfoToSatisfy != spendingInfo) {
      Future.fromTry(TxBuilderError.WrongSigner)
    } else {
      val (_, output, inputIndex, _) = relevantInfo(spendingInfo, unsignedTx)

      val wtx = WitnessTransaction.toWitnessTx(unsignedTx)

      val signedSigComponentF = BitcoinSigner.sign(
        spendingInfo,
        wtx,
        isDummySignature,
        spendingInfoToSatisfy.nestedSpendingInfo)

      val scriptWitF = signedSigComponentF.map { signedSigComponent =>
        P2WSHWitnessV0(spendingInfoToSatisfy.scriptWitness.redeemScript,
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

sealed abstract class LockTimeSignerSingle
    extends BitcoinSignerSingle[LockTimeSpendingInfoSingle] {

  override def signSingle(
      spendingInfo: UTXOSpendingInfoSingle,
      unsignedTx: Transaction,
      isDummySignature: Boolean,
      spendingInfoToSatisfy: LockTimeSpendingInfoSingle)(
      implicit ec: ExecutionContext): Future[
    (ECPublicKey, ECDigitalSignature)] = {
    BitcoinSignerSingle.signSingle(spendingInfo,
                                   unsignedTx,
                                   isDummySignature,
                                   spendingInfoToSatisfy.nestedSpendingInfo)
  }
}

object LockTimeSignerSingle extends LockTimeSignerSingle

sealed abstract class LockTimeSigner
    extends BitcoinSigner[LockTimeSpendingInfoFull] {

  override def sign(
      spendingInfo: UTXOSpendingInfo,
      unsignedTx: Transaction,
      isDummySignature: Boolean,
      spendingInfoToSatisfy: LockTimeSpendingInfoFull)(
      implicit ec: ExecutionContext): Future[TxSigComponent] = {
    BitcoinSigner.sign(spendingInfo,
                       unsignedTx,
                       isDummySignature,
                       spendingInfoToSatisfy.nestedSpendingInfo)
  }
}
object LockTimeSigner extends LockTimeSigner

sealed abstract class ConditionalSignerSingle
    extends BitcoinSignerSingle[ConditionalSpendingInfoSingle] {

  override def signSingle(
      spendingInfo: UTXOSpendingInfoSingle,
      unsignedTx: Transaction,
      isDummySignature: Boolean,
      spendingInfoToSatisfy: ConditionalSpendingInfoSingle)(
      implicit ec: ExecutionContext): Future[
    (ECPublicKey, ECDigitalSignature)] = {
    BitcoinSignerSingle.signSingle(spendingInfo,
                                   unsignedTx,
                                   isDummySignature,
                                   spendingInfoToSatisfy.nestedSpendingInfo)
  }
}

object ConditionalSignerSingle extends ConditionalSignerSingle

/** Delegates to get a ScriptSignature for the case being
  * spent and then adds an OP_TRUE or OP_FALSE
  */
sealed abstract class ConditionalSigner
    extends BitcoinSigner[ConditionalSpendingInfoFull] {

  override def sign(
      spendingInfo: UTXOSpendingInfo,
      unsignedTx: Transaction,
      isDummySignature: Boolean,
      spendingInfoToSatisfy: ConditionalSpendingInfoFull)(
      implicit ec: ExecutionContext): Future[TxSigComponent] = {
    val (_, output, inputIndex, _) = relevantInfo(spendingInfo, unsignedTx)

    val missingOpSigComponentF = BitcoinSigner.sign(
      spendingInfo,
      unsignedTx,
      isDummySignature,
      spendingInfoToSatisfy.nestedSpendingInfo)

    val scriptSigF = missingOpSigComponentF.map { sigComponent =>
      ConditionalScriptSignature(sigComponent.scriptSignature,
                                 spendingInfoToSatisfy.condition)
    }

    updateScriptSigInSigComponent(unsignedTx,
                                  inputIndex.toInt,
                                  output,
                                  scriptSigF)
  }
}
object ConditionalSigner extends ConditionalSigner
