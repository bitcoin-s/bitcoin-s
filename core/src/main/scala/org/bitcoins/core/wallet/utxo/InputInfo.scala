package org.bitcoins.core.wallet.utxo

import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.crypto.{ECPublicKey, NetworkElement, Sign}

/** An InputInfo contains all information other than private keys about
  * a particular spending condition in a UTXO.
  *
  * Note that while some pieces of information (TxOutPoint, amount, etc.)
  * apply to all input types, other pieces are specific to particular ones
  * such as a witness to a SegWit input.
  */
sealed trait InputInfo {
  def outPoint: TransactionOutPoint

  def prevTransaction: Transaction

  require(
    outPoint.txId == prevTransaction.txId,
    s"prevTransaction txId (${prevTransaction.txId.hex}) does not match the outPoint's (${outPoint.txId.hex})"
  )

  def amount: CurrencyUnit

  def scriptPubKey: ScriptPubKey

  def output: TransactionOutput = {
    TransactionOutput(amount, scriptPubKey)
  }

  // If using EmptyTransaction we are testing something else
  require(
    prevTransaction == EmptyTransaction || prevTransaction
      .outputs(outPoint.vout.toInt)
      .value == amount,
    s"prevTransaction output at index ${outPoint.vout.toInt} (${prevTransaction
      .outputs(outPoint.vout.toInt)}) does match the corresponding value $amount"
  )

  def outputReference: OutputReference = {
    OutputReference(outPoint, output)
  }

  def conditionalPath: ConditionalPath

  def pubKeys: Vector[ECPublicKey]

  def requiredSigs: Int

  def toSpendingInfo(
      signers: Vector[Sign],
      hashType: HashType): ScriptSignatureParams[InputInfo] = {
    ScriptSignatureParams(this, signers, hashType)
  }

  def toSpendingInfo(
      signer: Sign,
      hashType: HashType): ECSignatureParams[InputInfo] = {
    ECSignatureParams(this, signer, hashType)
  }

  def genericWithSignFrom(
      signerMaterial: InputSigningInfo[InputInfo]): InputSigningInfo[
    this.type] = {
    signerMaterial match {
      case info: ScriptSignatureParams[InputInfo] => withSignFrom(info)
      case info: ECSignatureParams[InputInfo]     => withSignFrom(info)
    }
  }

  def withSignFrom(
      signerMaterial: ScriptSignatureParams[InputInfo]): ScriptSignatureParams[
    this.type] = {
    signerMaterial.copy(inputInfo = this)
  }

  def withSignFrom(
      signerMaterial: ECSignatureParams[InputInfo]): ECSignatureParams[
    this.type] = {
    signerMaterial.copy(inputInfo = this)
  }
}

object InputInfo {

  def getRedeemScript(inputInfo: InputInfo): Option[ScriptPubKey] = {
    inputInfo match {
      case _: RawInputInfo | _: SegwitV0NativeInputInfo |
          _: UnassignedSegwitNativeInputInfo =>
        None
      case info: P2SHInputInfo => Some(info.redeemScript)
    }
  }

  def getScriptWitness(inputInfo: InputInfo): Option[ScriptWitness] = {
    inputInfo match {
      case _: RawInputInfo | _: P2SHNonSegwitInputInfo => None
      case info: SegwitV0NativeInputInfo               => Some(info.scriptWitness)
      case info: P2SHNestedSegwitV0InputInfo           => Some(info.scriptWitness)
      case info: UnassignedSegwitNativeInputInfo       => Some(info.scriptWitness)
    }
  }

  def getHashPreImages(inputInfo: InputInfo): Vector[NetworkElement] = {
    inputInfo match {
      case info: P2WSHV0InputInfo     => info.hashPreImages
      case info: P2SHInputInfo        => info.hashPreImages
      case info: P2PKHInputInfo       => Vector(info.pubKey)
      case info: LockTimeInputInfo    => info.hashPreImages
      case info: ConditionalInputInfo => info.hashPreImages
      case _: UnassignedSegwitNativeInputInfo | _: EmptyInputInfo |
          _: P2PKInputInfo | _: P2PKWithTimeoutInputInfo |
          _: MultiSignatureInputInfo | _: P2WPKHV0InputInfo =>
        Vector.empty
    }
  }

  def getPKHPreImage(inputInfo: InputInfo): Option[ECPublicKey] = {
    getHashPreImages(inputInfo).collectFirst {
      case pubKey: ECPublicKey => pubKey
    }
  }

  def apply(
      outPoint: TransactionOutPoint,
      prevTransaction: Transaction,
      output: TransactionOutput,
      redeemScriptOpt: Option[ScriptPubKey],
      scriptWitnessOpt: Option[ScriptWitness],
      conditionalPath: ConditionalPath,
      hashPreImages: Vector[NetworkElement] = Vector.empty): InputInfo = {
    output.scriptPubKey match {
      case _: P2SHScriptPubKey =>
        redeemScriptOpt match {
          case None =>
            throw new IllegalArgumentException(
              "Redeem Script must be defined for P2SH.")
          case Some(redeemScript) =>
            redeemScript match {
              case _: WitnessScriptPubKeyV0 =>
                val witness = scriptWitnessOpt match {
                  case Some(witness: ScriptWitnessV0) => witness
                  case None =>
                    throw new IllegalArgumentException(
                      "Script Witness must be defined for (nested) Segwit input")
                  case Some(_: ScriptWitness) =>
                    throw new UnsupportedOperationException(
                      "Only v0 Segwit is currently supported")
                }
                P2SHNestedSegwitV0InputInfo(outPoint,
                                            prevTransaction,
                                            output.value,
                                            witness,
                                            conditionalPath,
                                            hashPreImages)
              case nonWitnessSPK: RawScriptPubKey =>
                P2SHNonSegwitInputInfo(outPoint,
                                       prevTransaction,
                                       output.value,
                                       nonWitnessSPK,
                                       conditionalPath,
                                       hashPreImages)
              case _: P2SHScriptPubKey =>
                throw new IllegalArgumentException("Cannot have nested P2SH")
              case _: UnassignedWitnessScriptPubKey =>
                throw new UnsupportedOperationException(
                  s"Unsupported ScriptPubKey ${output.scriptPubKey}")
            }
        }
      case _: WitnessScriptPubKeyV0 =>
        val witness = scriptWitnessOpt match {
          case Some(witness: ScriptWitnessV0) => witness
          case None =>
            throw new IllegalArgumentException(
              "Script Witness must be defined for Segwit input")
          case Some(_: ScriptWitness) =>
            throw new UnsupportedOperationException(
              "Only v0 Segwit is currently supported")
        }
        SegwitV0NativeInputInfo(outPoint,
                                prevTransaction,
                                output.value,
                                witness,
                                conditionalPath,
                                hashPreImages)
      case wspk: UnassignedWitnessScriptPubKey =>
        UnassignedSegwitNativeInputInfo(
          outPoint,
          prevTransaction,
          output.value,
          wspk,
          scriptWitnessOpt.getOrElse(EmptyScriptWitness),
          conditionalPath,
          Vector.empty)
      case rawSPK: RawScriptPubKey =>
        RawInputInfo(outPoint,
                     prevTransaction,
                     output.value,
                     rawSPK,
                     conditionalPath,
                     hashPreImages)
    }
  }
}

sealed trait RawInputInfo extends InputInfo {
  override def scriptPubKey: RawScriptPubKey
}

object RawInputInfo {

  def apply(
      outPoint: TransactionOutPoint,
      prevTransaction: Transaction,
      amount: CurrencyUnit,
      scriptPubKey: RawScriptPubKey,
      conditionalPath: ConditionalPath,
      hashPreImages: Vector[NetworkElement] = Vector.empty): RawInputInfo = {
    scriptPubKey match {
      case p2pk: P2PKScriptPubKey =>
        P2PKInputInfo(outPoint, prevTransaction, amount, p2pk)
      case p2pkh: P2PKHScriptPubKey =>
        hashPreImages.collectFirst {
          case pubKey: ECPublicKey => pubKey
        } match {
          case None =>
            throw new IllegalArgumentException(
              "P2PKH pre-image must be specified for P2PKH ScriptPubKey")
          case Some(p2pkhPreImage) =>
            require(
              P2PKHScriptPubKey(p2pkhPreImage) == p2pkh,
              s"Specified P2PKH pre-image ($p2pkhPreImage) does not match $p2pkh")

            P2PKHInputInfo(outPoint, prevTransaction, amount, p2pkhPreImage)
        }
      case p2pkWithTimeout: P2PKWithTimeoutScriptPubKey =>
        conditionalPath.headOption match {
          case None =>
            throw new IllegalArgumentException(
              "ConditionalPath must be specified for P2PKWithTimeout")
          case Some(beforeTimeout) =>
            P2PKWithTimeoutInputInfo(outPoint,
                                     prevTransaction,
                                     amount,
                                     p2pkWithTimeout,
                                     beforeTimeout)
        }
      case multiSig: MultiSignatureScriptPubKey =>
        MultiSignatureInputInfo(outPoint, prevTransaction, amount, multiSig)
      case conditional: ConditionalScriptPubKey =>
        ConditionalInputInfo(outPoint,
                             prevTransaction,
                             amount,
                             conditional,
                             conditionalPath,
                             hashPreImages)
      case lockTime: LockTimeScriptPubKey =>
        LockTimeInputInfo(outPoint,
                          prevTransaction,
                          amount,
                          lockTime,
                          conditionalPath,
                          hashPreImages)
      case EmptyScriptPubKey =>
        EmptyInputInfo(outPoint, prevTransaction, amount)
      case _: NonStandardScriptPubKey | _: WitnessCommitment =>
        throw new UnsupportedOperationException(
          s"Currently unsupported ScriptPubKey $scriptPubKey")
    }
  }
}

case class EmptyInputInfo(
    outPoint: TransactionOutPoint,
    prevTransaction: Transaction,
    amount: CurrencyUnit)
    extends RawInputInfo {
  override def scriptPubKey: EmptyScriptPubKey.type = EmptyScriptPubKey

  override def conditionalPath: ConditionalPath =
    ConditionalPath.NoCondition
  override def pubKeys: Vector[ECPublicKey] = Vector.empty
  override def requiredSigs: Int = 0
}

case class P2PKInputInfo(
    outPoint: TransactionOutPoint,
    prevTransaction: Transaction,
    amount: CurrencyUnit,
    scriptPubKey: P2PKScriptPubKey)
    extends RawInputInfo {

  override def conditionalPath: ConditionalPath =
    ConditionalPath.NoCondition

  override def pubKeys: Vector[ECPublicKey] = Vector(scriptPubKey.publicKey)

  override def requiredSigs: Int = 1
}

case class P2PKHInputInfo(
    outPoint: TransactionOutPoint,
    prevTransaction: Transaction,
    amount: CurrencyUnit,
    pubKey: ECPublicKey)
    extends RawInputInfo {
  override def scriptPubKey: P2PKHScriptPubKey = P2PKHScriptPubKey(pubKey)

  override def conditionalPath: ConditionalPath =
    ConditionalPath.NoCondition

  override def pubKeys: Vector[ECPublicKey] = Vector(pubKey)

  override def requiredSigs: Int = 1
}

case class P2PKWithTimeoutInputInfo(
    outPoint: TransactionOutPoint,
    prevTransaction: Transaction,
    amount: CurrencyUnit,
    scriptPubKey: P2PKWithTimeoutScriptPubKey,
    isBeforeTimeout: Boolean)
    extends RawInputInfo {

  override def conditionalPath: ConditionalPath = {
    if (isBeforeTimeout) {
      ConditionalPath.nonNestedTrue
    } else {
      ConditionalPath.nonNestedFalse
    }
  }

  override def pubKeys: Vector[ECPublicKey] =
    Vector(scriptPubKey.pubKey, scriptPubKey.timeoutPubKey)

  override def requiredSigs: Int = 1
}

case class MultiSignatureInputInfo(
    outPoint: TransactionOutPoint,
    prevTransaction: Transaction,
    amount: CurrencyUnit,
    scriptPubKey: MultiSignatureScriptPubKey)
    extends RawInputInfo {

  override def conditionalPath: ConditionalPath =
    ConditionalPath.NoCondition

  override def pubKeys: Vector[ECPublicKey] = scriptPubKey.publicKeys.toVector

  override def requiredSigs: Int = scriptPubKey.requiredSigs
}

case class ConditionalInputInfo(
    outPoint: TransactionOutPoint,
    prevTransaction: Transaction,
    amount: CurrencyUnit,
    scriptPubKey: ConditionalScriptPubKey,
    conditionalPath: ConditionalPath,
    hashPreImages: Vector[NetworkElement] = Vector.empty)
    extends RawInputInfo {

  lazy val (condition: Boolean, nextConditionalPath: ConditionalPath) =
    conditionalPath match {
      case ConditionalPath.ConditionTrue(nextCondition) =>
        (true, nextCondition)
      case ConditionalPath.ConditionFalse(nextCondition) =>
        (false, nextCondition)
      case ConditionalPath.NoCondition =>
        throw new IllegalArgumentException("Must specify True or False")
    }

  val nestedInputInfo: RawInputInfo = {
    val nestedSPK = if (condition) {
      scriptPubKey.trueSPK
    } else {
      scriptPubKey.falseSPK
    }

    RawInputInfo(outPoint,
                 prevTransaction,
                 amount,
                 nestedSPK,
                 nextConditionalPath,
                 hashPreImages)
  }

  override def pubKeys: Vector[ECPublicKey] = nestedInputInfo.pubKeys

  override def requiredSigs: Int = nestedInputInfo.requiredSigs
}

case class LockTimeInputInfo(
    outPoint: TransactionOutPoint,
    prevTransaction: Transaction,
    amount: CurrencyUnit,
    scriptPubKey: LockTimeScriptPubKey,
    conditionalPath: ConditionalPath,
    hashPreImages: Vector[NetworkElement] = Vector.empty
) extends RawInputInfo {

  val nestedInputInfo: RawInputInfo = RawInputInfo(
    outPoint,
    prevTransaction: Transaction,
    amount,
    scriptPubKey.nestedScriptPubKey,
    conditionalPath,
    hashPreImages)

  override def pubKeys: Vector[ECPublicKey] = nestedInputInfo.pubKeys

  override def requiredSigs: Int = nestedInputInfo.requiredSigs
}

sealed trait SegwitV0NativeInputInfo extends InputInfo {
  def scriptWitness: ScriptWitnessV0
}

object SegwitV0NativeInputInfo {

  def apply(
      outPoint: TransactionOutPoint,
      prevTransaction: Transaction,
      amount: CurrencyUnit,
      scriptWitness: ScriptWitnessV0,
      conditionalPath: ConditionalPath,
      hashPreImages: Vector[NetworkElement] =
        Vector.empty): SegwitV0NativeInputInfo = {
    scriptWitness match {
      case p2wpkh: P2WPKHWitnessV0 =>
        P2WPKHV0InputInfo(outPoint, prevTransaction, amount, p2wpkh.pubKey)
      case p2wsh: P2WSHWitnessV0 =>
        P2WSHV0InputInfo(outPoint,
                         prevTransaction,
                         amount,
                         p2wsh,
                         conditionalPath,
                         hashPreImages)
    }
  }
}

case class P2WPKHV0InputInfo(
    outPoint: TransactionOutPoint,
    prevTransaction: Transaction,
    amount: CurrencyUnit,
    pubKey: ECPublicKey)
    extends SegwitV0NativeInputInfo {
  override def scriptPubKey: P2WPKHWitnessSPKV0 = P2WPKHWitnessSPKV0(pubKey)

  override def scriptWitness: P2WPKHWitnessV0 = P2WPKHWitnessV0(pubKey)

  override def conditionalPath: ConditionalPath =
    ConditionalPath.NoCondition

  override def pubKeys: Vector[ECPublicKey] = Vector(pubKey)

  override def requiredSigs: Int = 1
}

case class P2WSHV0InputInfo(
    outPoint: TransactionOutPoint,
    prevTransaction: Transaction,
    amount: CurrencyUnit,
    scriptWitness: P2WSHWitnessV0,
    conditionalPath: ConditionalPath,
    hashPreImages: Vector[NetworkElement] = Vector.empty)
    extends SegwitV0NativeInputInfo {

  override def scriptPubKey: P2WSHWitnessSPKV0 =
    P2WSHWitnessSPKV0(scriptWitness.redeemScript)

  val nestedInputInfo: RawInputInfo =
    RawInputInfo(outPoint,
                 prevTransaction: Transaction,
                 amount,
                 scriptWitness.redeemScript,
                 conditionalPath,
                 hashPreImages)

  override def pubKeys: Vector[ECPublicKey] = nestedInputInfo.pubKeys

  override def requiredSigs: Int = nestedInputInfo.requiredSigs
}

case class UnassignedSegwitNativeInputInfo(
    outPoint: TransactionOutPoint,
    prevTransaction: Transaction,
    amount: CurrencyUnit,
    scriptPubKey: WitnessScriptPubKey,
    scriptWitness: ScriptWitness,
    conditionalPath: ConditionalPath,
    pubKeys: Vector[ECPublicKey])
    extends InputInfo {
  override def requiredSigs: Int = pubKeys.length
}

sealed trait P2SHInputInfo extends InputInfo {
  def hashPreImages: Vector[NetworkElement]

  def redeemScript: ScriptPubKey

  override def scriptPubKey: P2SHScriptPubKey = P2SHScriptPubKey(redeemScript)

  def nestedInputInfo: InputInfo

  override def pubKeys: Vector[ECPublicKey] = nestedInputInfo.pubKeys

  override def requiredSigs: Int = nestedInputInfo.requiredSigs
}

case class P2SHNonSegwitInputInfo(
    outPoint: TransactionOutPoint,
    prevTransaction: Transaction,
    amount: CurrencyUnit,
    redeemScript: RawScriptPubKey,
    conditionalPath: ConditionalPath,
    hashPreImages: Vector[NetworkElement] = Vector.empty)
    extends P2SHInputInfo {

  override val nestedInputInfo: RawInputInfo =
    RawInputInfo(outPoint,
                 prevTransaction,
                 amount,
                 redeemScript,
                 conditionalPath,
                 hashPreImages)
}

case class P2SHNestedSegwitV0InputInfo(
    outPoint: TransactionOutPoint,
    prevTransaction: Transaction,
    amount: CurrencyUnit,
    scriptWitness: ScriptWitnessV0,
    conditionalPath: ConditionalPath,
    hashPreImages: Vector[NetworkElement] = Vector.empty)
    extends P2SHInputInfo {

  override def redeemScript: WitnessScriptPubKeyV0 =
    scriptWitness match {
      case p2wpkh: P2WPKHWitnessV0 => P2WPKHWitnessSPKV0(p2wpkh.pubKey)
      case p2wsh: P2WSHWitnessV0   => P2WSHWitnessSPKV0(p2wsh.redeemScript)
    }

  override val nestedInputInfo: SegwitV0NativeInputInfo =
    SegwitV0NativeInputInfo(outPoint,
                            prevTransaction,
                            amount,
                            scriptWitness,
                            conditionalPath,
                            hashPreImages)
}
