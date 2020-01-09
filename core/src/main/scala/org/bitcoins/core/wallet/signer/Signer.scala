package org.bitcoins.core.wallet.signer

import org.bitcoins.core.crypto._
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.script.flag.ScriptFlag
import org.bitcoins.core.wallet.builder.TxBuilderError
import org.bitcoins.core.wallet.utxo._
import scodec.bits.ByteVector

import scala.concurrent.{ExecutionContext, Future}

// TODO replace with InputPSBTRecord.PartialSignature when PSBTs are merged
case class PartialSignature(pubKey: ECPublicKey, sig: ECDigitalSignature)

/** The class used to represent a signing process for a specific [[org.bitcoins.core.protocol.script.ScriptPubKey]] type */
sealed abstract class Signer[-SpendInfo <: SpendingInfo] {

  /**
    * The method used to sign a bitcoin unspent transaction output
    * @param spendingInfo - The information required for signing
    * @param unsignedTx the external Transaction that needs an input signed
    * @param isDummySignature - do not sign the tx for real, just use a dummy signature this is useful for fee estimation
    * @return
    */
  def sign(
      spendingInfo: SpendInfo,
      unsignedTx: Transaction,
      isDummySignature: Boolean)(implicit ec: ExecutionContext): Future[Any] = { //FIXME this shouldn't be empty
    sign(
      spendingInfo = spendingInfo,
      unsignedTx = unsignedTx,
      isDummySignature = isDummySignature,
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
      spendingInfo: SpendingInfo,
      unsignedTx: Transaction,
      isDummySignature: Boolean,
      spendingInfoToSatisfy: SpendInfo)(implicit ec: ExecutionContext): Future[
    Any] //FIXME this shouldn't be empty

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
      spendingInfo: SpendingInfo,
      unsignedTx: Transaction): (Seq[Sign], TransactionOutput, UInt32, HashType) = {
    (spendingInfo.signers,
     spendingInfo.output,
     inputIndex(spendingInfo, unsignedTx),
     spendingInfo.hashType)
  }

  protected def inputIndex(
      spendingInfo: SpendingInfo,
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
      spendingInfo: SpendingInfo,
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
sealed abstract class BitcoinSigner[-SpendInfo <: BitcoinSpendingInfo]
    extends Signer[SpendInfo]

//object BitcoinSigner {
//
//  def sign(
//      spendingInfo: SpendingInfo,
//      unsignedTx: Transaction,
//      isDummySignature: Boolean)(
//      implicit ec: ExecutionContext): Future[TxSigComponent] = {
//    sign(spendingInfo, unsignedTx, isDummySignature, spendingInfo)
//  }
//
//  def sign(
//      spendingInfo: SpendingInfo,
//      unsignedTx: Transaction,
//      isDummySignature: Boolean,
//      spendingInfoToSatisfy: SpendingInfo)(
//      implicit ec: ExecutionContext): Future[TxSigComponent] = {
//    //todo
//    null
//  }
//}

sealed abstract class BitcoinPartialSigner[
    -SpendInfo <: BitcoinPartialSpendingInfo]
    extends BitcoinSigner[SpendInfo]

object BitcoinPartialSigner {

  def sign(
      spendingInfo: PartialSpendingInfo,
      unsignedTx: Transaction,
      isDummySignature: Boolean)(
      implicit ec: ExecutionContext): Future[PartialSignature] = {
    sign(spendingInfo, unsignedTx, isDummySignature, spendingInfo)
  }

  def sign(
      spendingInfo: PartialSpendingInfo,
      unsignedTx: Transaction,
      isDummySignature: Boolean,
      spendingInfoToSatisfy: PartialSpendingInfo)(
      implicit ec: ExecutionContext): Future[PartialSignature] = {
    spendingInfoToSatisfy match {
      case empty: EmptyPartialSpendingInfo =>
        EmptyPartialSigner.sign(spendingInfo,
                                unsignedTx,
                                isDummySignature,
                                empty)
      case p2pk: P2PKPartialSpendingInfo =>
        P2PKPartialSigner.sign(spendingInfo, unsignedTx, isDummySignature, p2pk)
      case p2pkh: P2PKHPartialSpendingInfo =>
        P2PKHPartialSigner.sign(spendingInfo,
                                unsignedTx,
                                isDummySignature,
                                p2pkh)
      case p2pKWithTimeout: P2PKWithTimeoutPartialSpendingInfo =>
        P2PKWithTimeoutPartialSigner.sign(spendingInfo,
                                          unsignedTx,
                                          isDummySignature,
                                          p2pKWithTimeout)
      case p2sh: P2SHPartialSpendingInfo =>
        P2SHPartialSigner.sign(spendingInfo, unsignedTx, isDummySignature, p2sh)
      case multiSig: MultiSignaturePartialSpendingInfo =>
        MultiSigPartialSigner
          .sign(spendingInfo, unsignedTx, isDummySignature, multiSig)
          .map(_.head) // fixme just taking head
      case lockTime: LockTimePartialSpendingInfo =>
        LockTimePartialSigner.sign(spendingInfo,
                                   unsignedTx,
                                   isDummySignature,
                                   lockTime)
      case conditional: ConditionalPartialSpendingInfo =>
        ConditionalPartialSigner.sign(spendingInfo,
                                      unsignedTx,
                                      isDummySignature,
                                      conditional)
      case p2wpkh: P2WPKHV0PartialSpendingInfo =>
        P2WPKHPartialSigner.sign(spendingInfo,
                                 unsignedTx,
                                 isDummySignature,
                                 p2wpkh)
      case pw2sh: P2WSHV0PartialSpendingInfo =>
        P2WSHPartialSigner.sign(spendingInfo,
                                unsignedTx,
                                isDummySignature,
                                pw2sh)
      case _: UnassignedSegwitNativePartialSpendingInfo =>
        throw new UnsupportedOperationException("Unsupported Segwit version")
    }
  }
}

/** For signing EmptyScriptPubKeys in tests, should probably not be used in real life. */
sealed abstract class EmptyPartialSigner
    extends BitcoinPartialSigner[EmptySpendingInfo] {

  override def sign(
      spendingInfo: SpendingInfo,
      unsignedTx: Transaction,
      isDummySignature: Boolean,
      spendingInfoToSatisfy: EmptySpendingInfo)(
      implicit ec: ExecutionContext): Future[PartialSignature] = {

    Future.successful(
      PartialSignature(ECPublicKey(ByteVector.empty),
                       ECDigitalSignature(ByteVector.empty)))
  }
}

object EmptyPartialSigner extends EmptyPartialSigner

/** Used to sign a [[org.bitcoins.core.protocol.script.P2PKScriptPubKey]] */
sealed abstract class P2PKPartialSigner
    extends BitcoinPartialSigner[P2PKSpendingInfo] {

  override def sign(
      spendingInfo: SpendingInfo,
      unsignedTx: Transaction,
      isDummySignature: Boolean,
      spendingInfoToSatisfy: P2PKSpendingInfo)(
      implicit ec: ExecutionContext): Future[PartialSignature] = {

    val signatureF = doSign(sigComponent(spendingInfo, unsignedTx),
                            spendingInfoToSatisfy.signer.signFunction,
                            spendingInfo.hashType,
                            isDummySignature)

    signatureF.map { sig =>
      PartialSignature(spendingInfoToSatisfy.signer.publicKey, sig)
    }
  }
}

object P2PKPartialSigner extends P2PKPartialSigner

/** Used to sign a [[org.bitcoins.core.protocol.script.P2PKHScriptPubKey]] */
sealed abstract class P2PKHPartialSigner
    extends BitcoinPartialSigner[P2PKHSpendingInfo] {

  override def sign(
      spendingInfo: SpendingInfo,
      unsignedTx: Transaction,
      isDummySignature: Boolean,
      spendingInfoToSatisfy: P2PKHSpendingInfo)(
      implicit ec: ExecutionContext): Future[PartialSignature] = {

    val signatureF = doSign(sigComponent(spendingInfo, unsignedTx),
                            spendingInfoToSatisfy.signer.signFunction,
                            spendingInfo.hashType,
                            isDummySignature)

    signatureF.map { sig =>
      PartialSignature(spendingInfoToSatisfy.signer.publicKey, sig)
    }
  }
}

object P2PKHPartialSigner extends P2PKHPartialSigner

/** Used to sign a [[org.bitcoins.core.protocol.script.P2PKHScriptPubKey]] */
sealed abstract class P2PKWithTimeoutPartialSigner
    extends BitcoinPartialSigner[P2PKWithTimeoutSpendingInfo] {

  override def sign(
      spendingInfo: SpendingInfo,
      unsignedTx: Transaction,
      isDummySignature: Boolean,
      spendingInfoToSatisfy: P2PKWithTimeoutSpendingInfo)(
      implicit ec: ExecutionContext): Future[PartialSignature] = {

    val signatureF = doSign(sigComponent(spendingInfo, unsignedTx),
                            spendingInfoToSatisfy.signer.signFunction,
                            spendingInfo.hashType,
                            isDummySignature)

    signatureF.map { sig =>
      PartialSignature(spendingInfoToSatisfy.signer.publicKey, sig)
    }
  }
}

object P2PKWithTimeoutPartialSigner extends P2PKWithTimeoutPartialSigner

/** Used to sign a [[org.bitcoins.core.protocol.script.P2SHScriptPubKey]] */
sealed abstract class P2SHPartialSigner
    extends BitcoinPartialSigner[P2SHSpendingInfo] {
  override def sign(
      spendingInfo: SpendingInfo,
      unsignedTx: Transaction,
      isDummySignature: Boolean,
      spendingInfoToSatisfy: P2SHSpendingInfo)(
      implicit ec: ExecutionContext): Future[PartialSignature] = {
    if (spendingInfoToSatisfy != spendingInfo) {
      Future.fromTry(TxBuilderError.WrongSigner)
    } else {
      val (_, _, inputIndex, _) = relevantInfo(spendingInfo, unsignedTx)

      val oldInput = unsignedTx.inputs(inputIndex.toInt)
      val input =
        TransactionInput(spendingInfo.outPoint,
                         EmptyScriptSignature,
                         oldInput.sequence)

      val updatedTx =
        unsignedTx.updateInput(inputIndex.toInt, input)

      BitcoinPartialSigner
        .sign(spendingInfoToSatisfy.nestedSpendingInfo,
              updatedTx,
              isDummySignature)
    }
  }
}

object P2SHPartialSigner extends P2SHPartialSigner

sealed abstract class MultiSigPartialSigner
    extends BitcoinPartialSigner[MultiSignatureSpendingInfo] {

  override def sign(
      spendingInfo: SpendingInfo,
      unsignedTx: Transaction,
      isDummySignature: Boolean,
      spendingInfoToSatisfy: MultiSignatureSpendingInfo)(
      implicit ec: ExecutionContext): Future[Vector[PartialSignature]] = { // TODO correct type?
    val (signersWithPubKeys, _, _, hashType) =
      relevantInfo(spendingInfo, unsignedTx)
    val signers = signersWithPubKeys.map(_.signFunction)

    val signatureFs = spendingInfoToSatisfy.signers.indices
      .map(
        i =>
          doSign(sigComponent(spendingInfo, unsignedTx),
                 signers(i),
                 hashType,
                 isDummySignature))

    val signaturesF = Future.sequence(signatureFs)

    signaturesF.map { sigs =>
      spendingInfoToSatisfy.signers.zip(sigs).map { sig =>
        PartialSignature(sig._1.publicKey, sig._2)
      }
    }
    null // fixme
  }
}

object MultiSigPartialSigner extends MultiSigPartialSigner

sealed abstract class P2WPKHPartialSigner
    extends BitcoinPartialSigner[P2WPKHV0SpendingInfo] {

  override def sign(
      spendingInfo: SpendingInfo,
      unsignedTx: Transaction,
      isDummySignature: Boolean,
      spendingInfoToSatisfy: P2WPKHV0SpendingInfo)(
      implicit ec: ExecutionContext): Future[PartialSignature] = {
    if (spendingInfoToSatisfy != spendingInfo) {
      Future.fromTry(TxBuilderError.WrongSigner)
    } else {
      val (_, output, inputIndex, hashType) =
        relevantInfo(spendingInfo, unsignedTx)
      unsignedTx match {
        case wtx: WitnessTransaction =>
          val signer = spendingInfoToSatisfy.signer

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

            val signatureF =
              doSign(wtxComp, signer.signFunction, hashType, isDummySignature)

            signatureF.map { sig =>
              PartialSignature(signer.publicKey, sig)
            }
          }
        case btx: BaseTransaction =>
          val wtx = WitnessTransaction.toWitnessTx(btx)

          BitcoinPartialSigner.sign(spendingInfoToSatisfy,
                                    wtx,
                                    isDummySignature)
      }
    }
  }
}
object P2WPKHPartialSigner extends P2WPKHPartialSigner

sealed abstract class LockTimePartialSigner
    extends BitcoinPartialSigner[LockTimeSpendingInfo] {

  override def sign(
      spendingInfo: SpendingInfo,
      unsignedTx: Transaction,
      isDummySignature: Boolean,
      spendingInfoToSatisfy: LockTimeSpendingInfo)(
      implicit ec: ExecutionContext): Future[PartialSignature] = {
    BitcoinPartialSigner.sign(spendingInfoToSatisfy,
                              unsignedTx,
                              isDummySignature,
                              spendingInfoToSatisfy.nestedSpendingInfo)
  }
}
object LockTimePartialSigner extends LockTimePartialSigner

/** Delegates to get a ScriptSignature for the case being
  * spent and then adds an OP_TRUE or OP_FALSE
  */
sealed abstract class ConditionalPartialSigner
    extends BitcoinPartialSigner[ConditionalSpendingInfo] {

  override def sign(
      spendingInfo: SpendingInfo,
      unsignedTx: Transaction,
      isDummySignature: Boolean,
      spendingInfoToSatisfy: ConditionalSpendingInfo)(
      implicit ec: ExecutionContext): Future[PartialSignature] = {
    BitcoinPartialSigner.sign(spendingInfoToSatisfy,
                              unsignedTx,
                              isDummySignature,
                              spendingInfoToSatisfy.nestedSpendingInfo)
  }
}
object ConditionalPartialSigner extends ConditionalPartialSigner

sealed abstract class P2WSHPartialSigner
    extends BitcoinPartialSigner[P2WSHV0SpendingInfo] {

  override def sign(
      spendingInfo: SpendingInfo,
      unsignedTx: Transaction,
      isDummySignature: Boolean,
      spendingInfoToSatisfy: P2WSHV0SpendingInfo)(
      implicit ec: ExecutionContext): Future[PartialSignature] = {
    if (spendingInfoToSatisfy != spendingInfo) {
      Future.fromTry(TxBuilderError.WrongSigner)
    } else {
      val wtx = WitnessTransaction.toWitnessTx(unsignedTx)

      BitcoinPartialSigner.sign(spendingInfoToSatisfy,
                                wtx,
                                isDummySignature,
                                spendingInfoToSatisfy.nestedSpendingInfo)
    }
  }
}
object P2WSHPartialSigner extends P2WSHPartialSigner

sealed abstract class BitcoinUTXOSigner[-SpendInfo <: BitcoinUTXOSpendingInfo]
    extends BitcoinSigner[SpendInfo] {
  override def sign(
      spendingInfo: SpendInfo,
      unsignedTx: Transaction,
      isDummySignature: Boolean)(
      implicit ec: ExecutionContext): Future[TxSigComponent] = {
    sign(
      spendingInfo = spendingInfo,
      unsignedTx = unsignedTx,
      isDummySignature = isDummySignature,
      spendingInfoToSatisfy = spendingInfo
    )
  }

  override def sign(
      spendingInfo: SpendingInfo,
      unsignedTx: Transaction,
      isDummySignature: Boolean,
      spendingInfoToSatisfy: SpendInfo)(
      implicit ec: ExecutionContext): Future[TxSigComponent]
}

object BitcoinUTXOSigner {

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
      case empty: EmptyUTXOSpendingInfo =>
        EmptyUTXOSigner.sign(spendingInfo, unsignedTx, isDummySignature, empty)
      case p2pk: P2PKUTXOSpendingInfo =>
        P2PKUTXOSigner.sign(spendingInfo, unsignedTx, isDummySignature, p2pk)
      case p2pkh: P2PKHUTXOSpendingInfo =>
        P2PKHUTXOSigner.sign(spendingInfo, unsignedTx, isDummySignature, p2pkh)
      case p2pKWithTimeout: P2PKWithTimeoutUTXOSpendingInfo =>
        P2PKWithTimeoutUTXOSigner.sign(spendingInfo,
                                       unsignedTx,
                                       isDummySignature,
                                       p2pKWithTimeout)
      case p2sh: P2SHUTXOSpendingInfo =>
        P2SHUTXOSigner.sign(spendingInfo, unsignedTx, isDummySignature, p2sh)
      case multiSig: MultiSignatureUTXOSpendingInfo =>
        MultiSigUTXOSigner.sign(spendingInfo,
                                unsignedTx,
                                isDummySignature,
                                multiSig)
      case lockTime: LockTimeUTXOSpendingInfo =>
        LockTimeUTXOSigner.sign(spendingInfo,
                                unsignedTx,
                                isDummySignature,
                                lockTime)
      case conditional: ConditionalUTXOSpendingInfo =>
        ConditionalUTXOSigner.sign(spendingInfo,
                                   unsignedTx,
                                   isDummySignature,
                                   conditional)
      case p2wpkh: P2WPKHV0UTXOSpendingInfo =>
        P2WPKHUTXOSigner.sign(spendingInfo,
                              unsignedTx,
                              isDummySignature,
                              p2wpkh)
      case pw2sh: P2WSHV0UTXOSpendingInfo =>
        P2WSHUTXOSigner.sign(spendingInfo, unsignedTx, isDummySignature, pw2sh)
      case _: UnassignedSegwitNativeUTXOSpendingInfo =>
        throw new UnsupportedOperationException("Unsupported Segwit version")
    }
  }
}

/** For signing EmptyScriptPubKeys in tests, should probably not be used in real life. */
sealed abstract class EmptyUTXOSigner
    extends BitcoinUTXOSigner[EmptyUTXOSpendingInfo] {

  override def sign(
      spendingInfo: SpendingInfo,
      unsignedTx: Transaction,
      isDummySignature: Boolean,
      spendingInfoToSatisfy: EmptyUTXOSpendingInfo)(
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

object EmptyUTXOSigner extends EmptyUTXOSigner

/** Used to sign a [[org.bitcoins.core.protocol.script.P2PKScriptPubKey]] */
sealed abstract class P2PKUTXOSigner
    extends BitcoinUTXOSigner[P2PKUTXOSpendingInfo] {

  override def sign(
      spendingInfo: SpendingInfo,
      unsignedTx: Transaction,
      isDummySignature: Boolean,
      spendingInfoToSatisfy: P2PKUTXOSpendingInfo)(
      implicit ec: ExecutionContext): Future[TxSigComponent] = {
    val (_, output, inputIndex, hashType) =
      relevantInfo(spendingInfo, unsignedTx)

    val signatureF = doSign(sigComponent(spendingInfo, unsignedTx),
                            spendingInfoToSatisfy.signer.signFunction,
                            hashType,
                            isDummySignature)

//    P2PKPartialSigner.sign(spendingInfoToSatisfy,
//      unsignedTx,
//      isDummySignature,
//      spendingInfoToSatisfy)

    val scriptSigF = signatureF.map { signature =>
      P2PKScriptSignature(signature)
    }

    updateScriptSigInSigComponent(unsignedTx,
                                  inputIndex.toInt,
                                  output,
                                  scriptSigF)
  }
}

object P2PKUTXOSigner extends P2PKUTXOSigner

/** Used to sign a [[org.bitcoins.core.protocol.script.P2PKHScriptPubKey]] */
sealed abstract class P2PKHUTXOSigner
    extends BitcoinUTXOSigner[P2PKHUTXOSpendingInfo] {

  override def sign(
      spendingInfo: SpendingInfo,
      unsignedTx: Transaction,
      isDummySignature: Boolean,
      spendingInfoToSatisfy: P2PKHUTXOSpendingInfo)(
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

//    P2PKHPartialSigner.sign(spendingInfoToSatisfy,
//                                  unsignedTx,
//                                  isDummySignature,
//                                  spendingInfoToSatisfy)

    val scriptSigF = signatureF.map { signature =>
      P2PKHScriptSignature(signature, pubKey)
    }

    updateScriptSigInSigComponent(unsignedTx,
                                  inputIndex.toInt,
                                  output,
                                  scriptSigF)
  }
}

object P2PKHUTXOSigner extends P2PKHUTXOSigner

sealed abstract class P2PKWithTimeoutUTXOSigner
    extends BitcoinUTXOSigner[P2PKWithTimeoutUTXOSpendingInfo] {
  override def sign(
      spendingInfo: SpendingInfo,
      unsignedTx: Transaction,
      isDummySignature: Boolean,
      spendingInfoToSatisfy: P2PKWithTimeoutUTXOSpendingInfo)(
      implicit ec: ExecutionContext): Future[TxSigComponent] = {
    val (signers, output, inputIndex, hashType) =
      relevantInfo(spendingInfo, unsignedTx)

    val sign = signers.head.signFunction

    val signatureF = doSign(sigComponent(spendingInfo, unsignedTx),
                            sign,
                            hashType,
                            isDummySignature)

    val scriptSigF = signatureF.map { signature =>
      P2PKWithTimeoutScriptSignature(spendingInfoToSatisfy.isBeforeTimeout,
                                     signature)
    }

    updateScriptSigInSigComponent(unsignedTx,
                                  inputIndex.toInt,
                                  output,
                                  scriptSigF) //TODO use partial signer
  }
}

object P2PKWithTimeoutUTXOSigner extends P2PKWithTimeoutUTXOSigner

sealed abstract class MultiSigUTXOSigner
    extends BitcoinUTXOSigner[MultiSignatureUTXOSpendingInfo] {

  override def sign(
      spendingInfo: SpendingInfo,
      unsignedTx: Transaction,
      isDummySignature: Boolean,
      spendingInfoToSatisfy: MultiSignatureUTXOSpendingInfo)(
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
                                  scriptSigF) //TODO use partial signer
  }
}

object MultiSigUTXOSigner extends MultiSigUTXOSigner

/** Used to sign a [[org.bitcoins.core.protocol.script.P2SHScriptPubKey]] */
sealed abstract class P2SHUTXOSigner
    extends BitcoinUTXOSigner[P2SHUTXOSpendingInfo] {
  override def sign(
      spendingInfo: SpendingInfo,
      unsignedTx: Transaction,
      isDummySignature: Boolean,
      spendingInfoToSatisfy: P2SHUTXOSpendingInfo)(
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
        BitcoinUTXOSigner
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
                           flags = flags) //TODO use partial signer
      }
    }
  }
}

object P2SHUTXOSigner extends P2SHUTXOSigner

sealed abstract class P2WPKHUTXOSigner
    extends BitcoinUTXOSigner[P2WPKHV0UTXOSpendingInfo] {

  override def sign(
      spendingInfo: SpendingInfo,
      unsignedTx: Transaction,
      isDummySignature: Boolean,
      spendingInfoToSatisfy: P2WPKHV0UTXOSpendingInfo)(
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

          BitcoinUTXOSigner.sign(spendingInfoToSatisfy, wtx, isDummySignature) //TODO use partial signer
      }
    }
  }
}
object P2WPKHUTXOSigner extends P2WPKHUTXOSigner

sealed abstract class P2WSHUTXOSigner
    extends BitcoinUTXOSigner[P2WSHV0UTXOSpendingInfo] {

  override def sign(
      spendingInfo: SpendingInfo,
      unsignedTx: Transaction,
      isDummySignature: Boolean,
      spendingInfoToSatisfy: P2WSHV0UTXOSpendingInfo)(
      implicit ec: ExecutionContext): Future[TxSigComponent] = {
    if (spendingInfoToSatisfy != spendingInfo) {
      Future.fromTry(TxBuilderError.WrongSigner)
    } else {
      val (_, output, inputIndex, _) = relevantInfo(spendingInfo, unsignedTx)

      val wtx = WitnessTransaction.toWitnessTx(unsignedTx)

      val signedSigComponentF = BitcoinUTXOSigner.sign(
        spendingInfoToSatisfy,
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
        WitnessTxSigComponentRaw(signedWTx, inputIndex, output, flags) //TODO use partial signer
      }
    }
  }
}
object P2WSHUTXOSigner extends P2WSHUTXOSigner

sealed abstract class LockTimeUTXOSigner
    extends BitcoinUTXOSigner[LockTimeUTXOSpendingInfo] {

  override def sign(
      spendingInfo: SpendingInfo,
      unsignedTx: Transaction,
      isDummySignature: Boolean,
      spendingInfoToSatisfy: LockTimeUTXOSpendingInfo)(
      implicit ec: ExecutionContext): Future[TxSigComponent] = {
    BitcoinUTXOSigner.sign(spendingInfoToSatisfy,
                           unsignedTx,
                           isDummySignature,
                           spendingInfoToSatisfy.nestedSpendingInfo)
  }
}
object LockTimeUTXOSigner extends LockTimeUTXOSigner

/** Delegates to get a ScriptSignature for the case being
  * spent and then adds an OP_TRUE or OP_FALSE
  */
sealed abstract class ConditionalUTXOSigner
    extends BitcoinUTXOSigner[ConditionalUTXOSpendingInfo] {

  override def sign(
      spendingInfo: SpendingInfo,
      unsignedTx: Transaction,
      isDummySignature: Boolean,
      spendingInfoToSatisfy: ConditionalUTXOSpendingInfo)(
      implicit ec: ExecutionContext): Future[TxSigComponent] = {
    val (_, output, inputIndex, _) = relevantInfo(spendingInfo, unsignedTx)

    val missingOpSigComponentF = BitcoinUTXOSigner.sign(
      spendingInfoToSatisfy,
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
                                  scriptSigF) //TODO use partial signer
  }
}
object ConditionalUTXOSigner extends ConditionalUTXOSigner
