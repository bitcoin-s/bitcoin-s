package org.bitcoins.core.wallet.utxo

import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.protocol.script.{
  SigVersionBase,
  SigVersionTaprootKeySpend,
  SigVersionWitnessV0,
  SignatureVersion
}
import org.bitcoins.core.protocol.transaction.*
import org.bitcoins.crypto.{
  ECPublicKey,
  HashType,
  PublicKey,
  SchnorrPublicKey,
  Sign,
  XOnlyPubKey
}

/** Stores the information required to generate a signature (ECSignatureParams)
  * or to generate a script signature (ScriptSignatureParams) for a given
  * satisfaction condition on a UTXO.
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

  private val keysToSignFor = inputInfo.pubKeys.map {
    case ec: ECPublicKey     => ec.toXOnly
    case s: SchnorrPublicKey => s.publicKey.toXOnly
    case x: XOnlyPubKey      => x
    case p: PublicKey        => sys.error(s"Not supported=$p")
  }
  require(
    signers.map(_.publicKey.toXOnly).forall(keysToSignFor.contains),
    s"Cannot have signers that do not sign for one of keysToSignFor=$keysToSignFor inputInfo.pubKeys=${inputInfo.pubKeys} signers=${signers
        .map(_.publicKey.toXOnly)} class=${inputInfo.getClass.getSimpleName}"
  )

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
      case _: TaprootKeyPathInputInfo => SigVersionTaprootKeySpend
      case i: InputInfo =>
        sys.error(s"Cannot determine SigVersion for unsupported inputInfo=$i")
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
      s"This method is for spending infos with a single signer, if you mean signers.head be explicit, signers=$signers")

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

  lazy val maxWitnessLen: Int = InputInfo.maxWitnessLen(inputInfo)

  lazy val maxScriptSigLen: Int = InputInfo.maxScriptSigLen(inputInfo)
}

object ScriptSignatureParams {

  def apply[InputType <: InputInfo](
      inputInfo: InputType,
      prevTransaction: Transaction,
      signer: Sign,
      hashType: HashType): ScriptSignatureParams[InputType] =
    ScriptSignatureParams(inputInfo, prevTransaction, Vector(signer), hashType)
}

/** Stores the information needed to generate an ECDigitalSignature for a use in
  * spending a UTXO.
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
