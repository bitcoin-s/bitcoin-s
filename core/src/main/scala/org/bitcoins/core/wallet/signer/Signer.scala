package org.bitcoins.core.wallet.signer

import org.bitcoins.core.crypto._
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.script.flag.ScriptFlag
import org.bitcoins.core.wallet.builder.TxBuilderError
import org.bitcoins.core.wallet.utxo.{
  BitcoinUTXOSpendingInfo,
  ConditionalSpendingInfo,
  LockTimeSpendingInfo,
  MultiSignatureSpendingInfo,
  P2PKHSpendingInfo,
  P2PKSpendingInfo,
  P2SHSpendingInfo,
  P2WPKHV0SpendingInfo,
  P2WSHV0SpendingInfo,
  UTXOSpendingInfo,
  UnassignedSegwitNativeUTXOSpendingInfo
}
import scodec.bits.ByteVector

import scala.concurrent.{ExecutionContext, Future}

/** The class used to represent a signing process for a specific [[org.bitcoins.core.protocol.script.ScriptPubKey]] type */
sealed abstract class Signer[-SpendingInfo <: UTXOSpendingInfo] {

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
      spendingInfo: UTXOSpendingInfo,
      unsignedTx: Transaction): (Seq[Sign], TransactionOutput, UInt32, HashType) = {
    (spendingInfo.signers,
     spendingInfo.output,
     inputIndex(spendingInfo, unsignedTx),
     spendingInfo.hashType)
  }

  protected def inputIndex(
      spendingInfo: UTXOSpendingInfo,
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
      spendingInfo: UTXOSpendingInfo,
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
              transactionWitnessOpt.getOrElse(EmptyWitness)

            WitnessTransaction(btx.version,
                               btx.inputs,
                               btx.outputs,
                               btx.lockTime,
                               transactionWitness)
          case wtx: WitnessTransaction => wtx
        }

        WitnessTxSigComponent(wtx, index, spendingInfo.output, flags)
      case _: P2SHScriptPubKey =>
        throw new IllegalStateException(
          "Signers do not currently interface with P2SH as this is handled externally in TxBuilder.scala"
        )
      case _: ScriptPubKey =>
        BaseTxSigComponent(unsignedTx, index, spendingInfo.output, flags)
    }
  }

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
      case p2pk: P2PKSpendingInfo =>
        P2PKSigner.sign(spendingInfo, unsignedTx, isDummySignature, p2pk)
      case p2pkh: P2PKHSpendingInfo =>
        P2PKHSigner.sign(spendingInfo, unsignedTx, isDummySignature, p2pkh)
      case multiSig: MultiSignatureSpendingInfo =>
        MultiSigSigner.sign(spendingInfo,
                            unsignedTx,
                            isDummySignature,
                            multiSig)
      case lockTime: LockTimeSpendingInfo =>
        LockTimeSigner.sign(spendingInfo,
                            unsignedTx,
                            isDummySignature,
                            lockTime)
      case conditional: ConditionalSpendingInfo =>
        ConditionalSigner.sign(spendingInfo,
                               unsignedTx,
                               isDummySignature,
                               conditional)
      case p2wpkh: P2WPKHV0SpendingInfo =>
        P2WPKHSigner.sign(spendingInfo, unsignedTx, isDummySignature, p2wpkh)
      case pw2sh: P2WSHV0SpendingInfo =>
        P2WSHSigner.sign(spendingInfo, unsignedTx, isDummySignature, pw2sh)
      case _: UnassignedSegwitNativeUTXOSpendingInfo =>
        throw new UnsupportedOperationException("Unsupported Segwit version")
      case _: P2SHSpendingInfo =>
        throw new IllegalArgumentException(
          "Signers do not currently interface with P2SH as this is handled externally in TxBuilder.scala")
    }
  }
}

/** Used to sign a [[org.bitcoins.core.protocol.script.P2PKScriptPubKey]] */
sealed abstract class P2PKSigner extends BitcoinSigner[P2PKSpendingInfo] {

  override def sign(
      spendingInfo: UTXOSpendingInfo,
      unsignedTx: Transaction,
      isDummySignature: Boolean,
      spendingInfoToSatisfy: P2PKSpendingInfo)(
      implicit ec: ExecutionContext): Future[TxSigComponent] = {
    val (_, output, inputIndex, hashType) =
      relevantInfo(spendingInfo, unsignedTx)

    val signatureF = doSign(sigComponent(spendingInfo, unsignedTx),
                            spendingInfoToSatisfy.signer.signFunction,
                            hashType,
                            isDummySignature)

    val scriptSigF = signatureF.map { signature =>
      P2PKScriptSignature(signature)
    }

    updateScriptSigInSigComponent(unsignedTx,
                                  inputIndex.toInt,
                                  output,
                                  scriptSigF)
  }
}

object P2PKSigner extends P2PKSigner

/** Used to sign a [[org.bitcoins.core.protocol.script.P2PKHScriptPubKey]] */
sealed abstract class P2PKHSigner extends BitcoinSigner[P2PKHSpendingInfo] {

  override def sign(
      spendingInfo: UTXOSpendingInfo,
      unsignedTx: Transaction,
      isDummySignature: Boolean,
      spendingInfoToSatisfy: P2PKHSpendingInfo)(
      implicit ec: ExecutionContext): Future[TxSigComponent] = {
    val (signers, output, inputIndex, hashType) =
      relevantInfo(spendingInfo, unsignedTx)

    val sign = signers.head.signFunction
    val pubKey = signers.head.publicKey

    val signatureF =
      doSign(sigComponent(spendingInfo, unsignedTx),
             sign,
             hashType,
             isDummySignature)

    val scriptSigF = signatureF.map { signature =>
      P2PKHScriptSignature(signature, pubKey)
    }

    updateScriptSigInSigComponent(unsignedTx,
                                  inputIndex.toInt,
                                  output,
                                  scriptSigF)
  }
}

object P2PKHSigner extends P2PKHSigner

sealed abstract class MultiSigSigner
    extends BitcoinSigner[MultiSignatureSpendingInfo] {

  override def sign(
      spendingInfo: UTXOSpendingInfo,
      unsignedTx: Transaction,
      isDummySignature: Boolean,
      spendingInfoToSatisfy: MultiSignatureSpendingInfo)(
      implicit ec: ExecutionContext): Future[TxSigComponent] = {
    val (signersWithPubKeys, output, inputIndex, hashType) =
      relevantInfo(spendingInfo, unsignedTx)
    val signers = signersWithPubKeys.map(_.signFunction)

    val requiredSigs = spendingInfoToSatisfy.scriptPubKey.requiredSigs
    val signatureFs = 0
      .until(requiredSigs)
      .map(
        i =>
          doSign(sigComponent(spendingInfo, unsignedTx),
                 signers(i),
                 hashType,
                 isDummySignature))

    val signaturesF = Future.sequence(signatureFs)

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

sealed abstract class P2WPKHSigner extends BitcoinSigner[P2WPKHV0SpendingInfo] {

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
            case p2wpkh: P2WPKHWitnessSPKV0 =>
              if (p2wpkh != P2WPKHWitnessSPKV0(pubKey)) {
                Future.fromTry(TxBuilderError.WrongPublicKey)
              } else Future.successful(p2wpkh)
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
          val wtx = WitnessTransaction(btx.version,
                                       btx.inputs,
                                       btx.outputs,
                                       btx.lockTime,
                                       EmptyWitness)

          sign(spendingInfoToSatisfy, wtx, isDummySignature)
      }
    }
  }
}
object P2WPKHSigner extends P2WPKHSigner

sealed abstract class P2WSHSigner extends BitcoinSigner[P2WSHV0SpendingInfo] {

  override def sign(
      spendingInfo: UTXOSpendingInfo,
      unsignedTx: Transaction,
      isDummySignature: Boolean,
      spendingInfoToSatisfy: P2WSHV0SpendingInfo)(
      implicit ec: ExecutionContext): Future[TxSigComponent] = {
    if (spendingInfoToSatisfy != spendingInfo) {
      Future.fromTry(TxBuilderError.WrongSigner)
    } else {
      val (_, output, inputIndex, _) = relevantInfo(spendingInfo, unsignedTx)

      val wtx = unsignedTx match {
        case btx: BaseTransaction =>
          WitnessTransaction(btx.version,
                             btx.inputs,
                             btx.outputs,
                             btx.lockTime,
                             EmptyWitness)
        case wtx: WitnessTransaction => wtx
      }

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

sealed abstract class LockTimeSigner
    extends BitcoinSigner[LockTimeSpendingInfo] {

  override def sign(
      spendingInfo: UTXOSpendingInfo,
      unsignedTx: Transaction,
      isDummySignature: Boolean,
      spendingInfoToSatisfy: LockTimeSpendingInfo)(
      implicit ec: ExecutionContext): Future[TxSigComponent] = {
    BitcoinSigner.sign(spendingInfo,
                       unsignedTx,
                       isDummySignature,
                       spendingInfoToSatisfy.nestedSpendingInfo)
  }
}
object LockTimeSigner extends LockTimeSigner

/** Delegates to get a ScriptSignature for the case being
  * spent and then adds an OP_TRUE or OP_FALSE
  */
sealed abstract class ConditionalSigner
    extends BitcoinSigner[ConditionalSpendingInfo] {

  override def sign(
      spendingInfo: UTXOSpendingInfo,
      unsignedTx: Transaction,
      isDummySignature: Boolean,
      spendingInfoToSatisfy: ConditionalSpendingInfo)(
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
