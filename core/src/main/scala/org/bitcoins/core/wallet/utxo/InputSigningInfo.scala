package org.bitcoins.core.wallet.utxo

import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.protocol.transaction.{
  OutputReference,
  TransactionOutPoint,
  TransactionOutput
}
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.crypto.Sign

/** Stores the information required to generate a signature (ECSignatureParams)
  * or to generate a script signature (ScriptSignatureParams) for a given satisfaction
  * condition on a UTXO.
  */
sealed trait InputSigningInfo[+InputType <: InputInfo] {
  def inputInfo: InputType
  def hashType: HashType
  def signers: Vector[Sign]

  private val keysToSignFor = inputInfo.pubKeys
  require(signers.map(_.publicKey).forall(keysToSignFor.contains),
          s"Cannot have signers that do not sign for one of $keysToSignFor")

  def outputReference: OutputReference = inputInfo.outputReference
  def amount: CurrencyUnit = inputInfo.amount
  def output: TransactionOutput = inputInfo.output
  def outPoint: TransactionOutPoint = inputInfo.outPoint
  def conditionalPath: ConditionalPath = inputInfo.conditionalPath
}

/** Stores the information needed to generate a ScriptSignature for a specific
  * spending condition on a UTXO.
  */
case class ScriptSignatureParams[+InputType <: InputInfo](
    inputInfo: InputType,
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
    ECSignatureParams(inputInfo, signers(index), hashType)
  }

  def toSingles: Vector[ECSignatureParams[InputType]] = {
    signers.map { signer =>
      ECSignatureParams(inputInfo, signer, hashType)
    }
  }

  def mapInfo[T <: InputInfo](
      func: InputType => T): ScriptSignatureParams[T] = {
    this.copy(inputInfo = func(this.inputInfo))
  }
}

object ScriptSignatureParams {

  def apply[InputType <: InputInfo](
      inputInfo: InputType,
      signer: Sign,
      hashType: HashType): ScriptSignatureParams[InputType] =
    ScriptSignatureParams(inputInfo, Vector(signer), hashType)
}

/** Stores the information needed to generate an ECDigitalSignature for
  * a use in spending a UTXO.
  */
case class ECSignatureParams[+InputType <: InputInfo](
    inputInfo: InputType,
    signer: Sign,
    hashType: HashType)
    extends InputSigningInfo[InputType] {
  override def signers: Vector[Sign] = Vector(signer)

  def toScriptSignatureParams: ScriptSignatureParams[InputType] = {
    ScriptSignatureParams(inputInfo, signer, hashType)
  }

  def mapInfo[T <: InputInfo](func: InputType => T): ECSignatureParams[T] = {
    this.copy(inputInfo = func(this.inputInfo))
  }
}
