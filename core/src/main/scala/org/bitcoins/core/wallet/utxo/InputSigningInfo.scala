package org.bitcoins.core.wallet.utxo

import org.bitcoins.core.currency.{CurrencyUnit, Satoshis}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.wallet.signer.BitcoinSigner
import org.bitcoins.crypto.Sign

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext}

/** Stores the information required to generate a signature (ECSignatureParams)
  * or to generate a script signature (ScriptSignatureParams) for a given satisfaction
  * condition on a UTXO.
  */
sealed trait InputSigningInfo[+InputType <: InputInfo] {
  def inputInfo: InputType
  def prevTransaction: Transaction
  def hashType: HashType
  def signers: Vector[Sign]

  // If using EmptyTransaction we are testing or dummy signing
  require(
    prevTransaction == EmptyTransaction || outPoint.txId == prevTransaction.txId,
    s"prevTransaction txId (${prevTransaction.txId.hex}) does not match the outPoint's (${outPoint.txId.hex})"
  )

  require(
    prevTransaction == EmptyTransaction || prevTransaction
      .outputs(outPoint.vout.toInt)
      .value == amount,
    s"prevTransaction output at index ${outPoint.vout.toInt} (${prevTransaction
      .outputs(outPoint.vout.toInt)}) does match the corresponding value $amount"
  )

  private val keysToSignFor = inputInfo.pubKeys
  require(signers.map(_.publicKey).forall(keysToSignFor.contains),
          s"Cannot have signers that do not sign for one of $keysToSignFor")

  def outputReference: OutputReference = inputInfo.outputReference
  def amount: CurrencyUnit = inputInfo.amount
  def output: TransactionOutput = inputInfo.output
  def outPoint: TransactionOutPoint = inputInfo.outPoint
  def conditionalPath: ConditionalPath = inputInfo.conditionalPath

  def sigVersion: SignatureVersion =
    inputInfo match {
      case _: SegwitV0NativeInputInfo | _: UnassignedSegwitNativeInputInfo |
          _: P2SHNestedSegwitV0InputInfo =>
        SigVersionWitnessV0
      case _: P2SHNonSegwitInputInfo | _: RawInputInfo =>
        SigVersionBase
    }
}

/** Stores the information needed to generate a ScriptSignature for a specific
  * spending condition on a UTXO.
  */
case class ScriptSignatureParams[+InputType <: InputInfo](
    inputInfo: InputType,
    prevTransaction: Transaction,
    signers: Vector[Sign],
    hashType: HashType)
    extends InputSigningInfo[InputType] {

  def signer: Sign = {
    require(
      signers.length == 1,
      "This method is for spending infos with a single signer, if you mean signers.head be explicit")

    signers.head
  }

  def toSingle(index: Int): ECSignatureParams[InputType] = {
    ECSignatureParams(inputInfo, prevTransaction, signers(index), hashType)
  }

  def toSingles: Vector[ECSignatureParams[InputType]] = {
    signers.map { signer =>
      ECSignatureParams(inputInfo, prevTransaction, signer, hashType)
    }
  }

  def mapInfo[T <: InputInfo](
      func: InputType => T): ScriptSignatureParams[T] = {
    this.copy(inputInfo = func(this.inputInfo))
  }

  def maxScriptSigAndWitnessWeight(implicit
      ec: ExecutionContext): (Long, Long) = {
    val dummyTx = BaseTransaction(
      TransactionConstants.validLockVersion,
      Vector(
        TransactionInput(inputInfo.outPoint,
                         EmptyScriptSignature,
                         UInt32.zero)),
      Vector(TransactionOutput(Satoshis.zero, EmptyScriptPubKey)),
      UInt32.zero
    )

    val maxWitnessLenF = BitcoinSigner
      .sign(this, unsignedTx = dummyTx, isDummySignature = true)
      .map(_.transaction)
      .map {
        case wtx: WitnessTransaction =>
          val scriptSigSize = wtx.inputs.head.scriptSignature.asmBytes.size
          val witnessSize = wtx.witness.head.byteSize
          (scriptSigSize * 4, witnessSize)
        case tx: NonWitnessTransaction =>
          val scriptSigSize = tx.inputs.head.scriptSignature.asmBytes.size
          (scriptSigSize * 4, 0L)
      }

    Await.result(maxWitnessLenF, 30.seconds)
  }
}

object ScriptSignatureParams {

  def apply[InputType <: InputInfo](
      inputInfo: InputType,
      prevTransaction: Transaction,
      signer: Sign,
      hashType: HashType): ScriptSignatureParams[InputType] =
    ScriptSignatureParams(inputInfo, prevTransaction, Vector(signer), hashType)
}

/** Stores the information needed to generate an ECDigitalSignature for
  * a use in spending a UTXO.
  */
case class ECSignatureParams[+InputType <: InputInfo](
    inputInfo: InputType,
    prevTransaction: Transaction,
    signer: Sign,
    hashType: HashType)
    extends InputSigningInfo[InputType] {
  override def signers: Vector[Sign] = Vector(signer)

  def toScriptSignatureParams: ScriptSignatureParams[InputType] = {
    ScriptSignatureParams(inputInfo, prevTransaction, signer, hashType)
  }

  def mapInfo[T <: InputInfo](func: InputType => T): ECSignatureParams[T] = {
    this.copy(inputInfo = func(this.inputInfo))
  }
}
