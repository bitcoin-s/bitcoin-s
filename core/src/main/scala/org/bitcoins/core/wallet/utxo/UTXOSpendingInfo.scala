package org.bitcoins.core.wallet.utxo

import org.bitcoins.core.crypto.Sign
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.protocol.script.{
  ConditionalScriptPubKey,
  EmptyScriptPubKey,
  EmptyScriptWitness,
  LockTimeScriptPubKey,
  MultiSignatureScriptPubKey,
  NonStandardScriptPubKey,
  P2PKHScriptPubKey,
  P2PKScriptPubKey,
  P2PKWithTimeoutScriptPubKey,
  P2SHScriptPubKey,
  P2WPKHWitnessSPKV0,
  P2WPKHWitnessV0,
  P2WSHWitnessSPKV0,
  P2WSHWitnessV0,
  RawScriptPubKey,
  ScriptPubKey,
  ScriptWitness,
  ScriptWitnessV0,
  UnassignedWitnessScriptPubKey,
  WitnessCommitment,
  WitnessScriptPubKey,
  WitnessScriptPubKeyV0
}
import org.bitcoins.core.protocol.transaction.{
  TransactionOutPoint,
  TransactionOutput
}
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.util.CryptoUtil

/**
  * Contains the information required to sign an unspent transaction output (UTXO) for a single key
  */
sealed abstract class UTXOSpendingInfoSingle {

  /** The funding transaction's txid and the index of the output in the transaction we are spending */
  def outPoint: TransactionOutPoint

  def amount: CurrencyUnit

  def scriptPubKey: ScriptPubKey

  /** the actual output itself we are spending */
  def output: TransactionOutput = {
    TransactionOutput(value = amount, scriptPubKey = scriptPubKey)
  }

  /** The signer signing in the output above */
  def signer: Sign

  def hashType: HashType

  def redeemScriptOpt: Option[ScriptPubKey]

  def scriptWitnessOpt: Option[ScriptWitness]

  def conditionalPath: ConditionalPath
}

/**
  * Contains the information required to spend an unspent transaction output (UTXO)
  * on a blockchain.
  */
sealed abstract class UTXOSpendingInfo extends UTXOSpendingInfoSingle {

  /** All signers needed to spend from the output above */
  def signers: Seq[Sign]

  /** If a UTXOSpendingInfo is called where a UTXOSpendingInfoSingle is used,
    * the first key is used.
    * This is nice because it allows us to use things like P2PKHSpendingInfo in both places
    * where full signing and single key singing happen.
    */
  override def signer: Sign = signers.head
}

/** Represents the spending branch being taken in a ScriptPubKey's execution
  *
  * If you over-specify a path, such as giving a condition where none is needed,
  * then the remaining over-specified path will be ignored.
  *
  * For example, if you wanted to spend a ConditionalScriptPubKey(P2PK1, P2PK2)
  * (which looks like OP_IF <P2PK1> OP_ELSE <P2PK2> OP_ENDIF) with the P2PK1 case,
  * then you would construct a ConditionalSpendingInfo using nonNestedTrue as your
  * ConditionalPath. Otherwise if you wanted to use P2PK2 you would use nonNestedFalse.
  */
sealed trait ConditionalPath {
  def headOption: Option[Boolean]
}

object ConditionalPath {
  case object NoConditionsLeft extends ConditionalPath {
    override val headOption: Option[Boolean] = None
  }
  case class ConditionTrue(nextCondition: ConditionalPath)
      extends ConditionalPath {
    override val headOption: Option[Boolean] = Some(true)
  }
  case class ConditionFalse(nextCondition: ConditionalPath)
      extends ConditionalPath {
    override val headOption: Option[Boolean] = Some(false)
  }

  val nonNestedTrue: ConditionalPath = ConditionTrue(NoConditionsLeft)
  val nonNestedFalse: ConditionalPath = ConditionFalse(NoConditionsLeft)
}

sealed trait BitcoinUTXOSpendingInfoSingle extends UTXOSpendingInfoSingle {

  protected def isValidScriptWitness(
      spk: WitnessScriptPubKeyV0,
      scriptWitness: ScriptWitnessV0): Boolean =
    BitcoinUTXOSpendingInfo.isValidScriptWitness(spk, scriptWitness)
}

sealed trait BitcoinUTXOSpendingInfo
    extends UTXOSpendingInfo
    with BitcoinUTXOSpendingInfoSingle

object BitcoinUTXOSpendingInfo {

  def apply(
      outPoint: TransactionOutPoint,
      output: TransactionOutput,
      signers: Seq[Sign],
      redeemScriptOpt: Option[ScriptPubKey],
      scriptWitnessOpt: Option[ScriptWitness],
      hashType: HashType,
      conditionalPath: ConditionalPath): BitcoinUTXOSpendingInfo = {
    output.scriptPubKey match {
      case p2sh: P2SHScriptPubKey =>
        redeemScriptOpt match {
          case None =>
            throw new IllegalArgumentException(
              "Redeem Script must be defined for P2SH.")
          case Some(redeemScript) =>
            redeemScript match {
              case wspk: WitnessScriptPubKeyV0 =>
                val witnessOpt = scriptWitnessOpt match {
                  case Some(witness: ScriptWitnessV0) => Some(witness)
                  case None                           => None
                  case Some(_: ScriptWitness) =>
                    throw new UnsupportedOperationException(
                      "Only v0 Segwit is currently supported")
                }
                P2SHNestedSegwitV0UTXOSpendingInfoFull(
                  outPoint,
                  output.value,
                  p2sh,
                  signers,
                  hashType,
                  wspk,
                  witnessOpt.getOrElse(throw new IllegalArgumentException(
                    "Script Witness must be defined for (nested) Segwit input")),
                  conditionalPath
                )
              case nonWitnessSPK: RawScriptPubKey =>
                P2SHNoNestSpendingInfoFull(outPoint,
                                           output.value,
                                           p2sh,
                                           signers,
                                           hashType,
                                           nonWitnessSPK,
                                           conditionalPath)
              case _: P2SHScriptPubKey =>
                throw new IllegalArgumentException("Cannot have nested P2SH")
              case _: UnassignedWitnessScriptPubKey =>
                throw new UnsupportedOperationException(
                  s"Unsupported ScriptPubKey ${output.scriptPubKey}")
            }
        }
      case wspk: WitnessScriptPubKeyV0 =>
        val witnessOpt = scriptWitnessOpt match {
          case Some(witness: ScriptWitnessV0) => Some(witness)
          case None                           => None
          case Some(_: ScriptWitness) =>
            throw new UnsupportedOperationException(
              "Only v0 Segwit is currently supported")
        }

        SegwitV0NativeUTXOSpendingInfo(
          outPoint,
          output.value,
          wspk,
          signers,
          hashType,
          witnessOpt.getOrElse(
            throw new IllegalArgumentException(
              "Script Witness must be defined for Segwit input")),
          conditionalPath
        )
      case wspk: UnassignedWitnessScriptPubKey =>
        UnassignedSegwitNativeUTXOSpendingInfo(
          outPoint,
          output.value,
          wspk,
          signers,
          hashType,
          scriptWitnessOpt.getOrElse(EmptyScriptWitness),
          conditionalPath)
      case p2pk: P2PKScriptPubKey =>
        P2PKSpendingInfo(outPoint, output.value, p2pk, signers.head, hashType)
      case p2pkh: P2PKHScriptPubKey =>
        P2PKHSpendingInfo(outPoint, output.value, p2pkh, signers.head, hashType)
      case p2pkWithTimeout: P2PKWithTimeoutScriptPubKey =>
        conditionalPath.headOption match {
          case None =>
            throw new IllegalArgumentException(
              "ConditionalPath must be specified for P2PKWithTimeout")
          case Some(beforeTimeout) =>
            P2PKWithTimeoutSpendingInfo(outPoint,
                                        output.value,
                                        p2pkWithTimeout,
                                        signers.head,
                                        hashType,
                                        beforeTimeout)
        }
      case multisig: MultiSignatureScriptPubKey =>
        MultiSignatureSpendingInfoFull(outPoint,
                                       output.value,
                                       multisig,
                                       signers.toVector,
                                       hashType)
      case locktime: LockTimeScriptPubKey =>
        LockTimeSpendingInfoFull(outPoint,
                                 output.value,
                                 locktime,
                                 signers.toVector,
                                 hashType,
                                 conditionalPath)
      case conditional: ConditionalScriptPubKey =>
        ConditionalSpendingInfoFull(outPoint,
                                    output.value,
                                    conditional,
                                    signers.toVector,
                                    hashType,
                                    conditionalPath)
      case _: NonStandardScriptPubKey | _: WitnessCommitment |
          EmptyScriptPubKey =>
        throw new UnsupportedOperationException(
          s"Currently unsupported ScriptPubKey ${output.scriptPubKey}")
    }
  }

  def unapply(info: BitcoinUTXOSpendingInfo): Option[
    (
        TransactionOutPoint,
        TransactionOutput,
        Seq[Sign],
        Option[ScriptPubKey],
        Option[ScriptWitness],
        HashType,
        ConditionalPath)] = {
    Some(info.outPoint,
         info.output,
         info.signers,
         info.redeemScriptOpt,
         info.scriptWitnessOpt,
         info.hashType,
         info.conditionalPath)
  }

  private[utxo] def isValidScriptWitness(
      spk: WitnessScriptPubKeyV0,
      scriptWitness: ScriptWitnessV0): Boolean = {
    spk match {
      case p2wpkh: P2WPKHWitnessSPKV0 =>
        scriptWitness match {
          case witness: P2WPKHWitnessV0 =>
            CryptoUtil.sha256Hash160(witness.pubKey.bytes) == p2wpkh.pubKeyHash
          case _: ScriptWitnessV0 => false
        }
      case p2wsh: P2WSHWitnessSPKV0 =>
        scriptWitness match {
          case witness: P2WSHWitnessV0 =>
            CryptoUtil.sha256(witness.redeemScript.asmBytes) == p2wsh.scriptHash
          case _: ScriptWitnessV0 => false
        }
    }
  }
}

sealed trait RawScriptUTXOSpendingInfoSingle
    extends BitcoinUTXOSpendingInfoSingle {
  override def scriptPubKey: RawScriptPubKey

  override val redeemScriptOpt: Option[ScriptPubKey] = None

  override val scriptWitnessOpt: Option[ScriptWitnessV0] = None
}

object RawScriptUTXOSpendingInfoSingle {

  private[utxo] def shouldBeFull(
      spendingInfo: RawScriptUTXOSpendingInfoSingle): Boolean = {
    spendingInfo match {
      case _: RawScriptUTXOSpendingInfo        => true
      case _: MultiSignatureSpendingInfoSingle => false
      case conditional: ConditionalSpendingInfoSingle =>
        conditional.shouldBeFull
      case lockTime: LockTimeSpendingInfoSingle =>
        lockTime.shouldBeFull
    }
  }

  def apply(
      outPoint: TransactionOutPoint,
      amount: CurrencyUnit,
      scriptPubKey: RawScriptPubKey,
      signer: Sign,
      hashType: HashType,
      conditionalPath: ConditionalPath): RawScriptUTXOSpendingInfoSingle = {
    ???
  }
}

/** This represents the information needed to be spend scripts like
  * [[org.bitcoins.core.protocol.script.P2PKHScriptPubKey p2pkh]] or [[org.bitcoins.core.protocol.script.P2PKScriptPubKey p2pk]]
  * scripts. Basically there is no nesting that requires a redeem script here*/
sealed trait RawScriptUTXOSpendingInfo
    extends BitcoinUTXOSpendingInfo
    with RawScriptUTXOSpendingInfoSingle

object RawScriptUTXOSpendingInfo {

  def apply(
      outPoint: TransactionOutPoint,
      amount: CurrencyUnit,
      scriptPubKey: RawScriptPubKey,
      signers: Seq[Sign],
      hashType: HashType,
      conditionalPath: ConditionalPath): RawScriptUTXOSpendingInfo = {
    scriptPubKey match {
      case p2pk: P2PKScriptPubKey =>
        P2PKSpendingInfo(outPoint, amount, p2pk, signers.head, hashType)
      case p2pkh: P2PKHScriptPubKey =>
        P2PKHSpendingInfo(outPoint, amount, p2pkh, signers.head, hashType)
      case p2pkWithTimeout: P2PKWithTimeoutScriptPubKey =>
        conditionalPath.headOption match {
          case None =>
            throw new IllegalArgumentException(
              "ConditionalPath must be specified for P2PKWithTimeout")
          case Some(beforeTimeout) =>
            P2PKWithTimeoutSpendingInfo(outPoint,
                                        amount,
                                        p2pkWithTimeout,
                                        signers.head,
                                        hashType,
                                        beforeTimeout)
        }
      case multisig: MultiSignatureScriptPubKey =>
        MultiSignatureSpendingInfoFull(outPoint,
                                       amount,
                                       multisig,
                                       signers.toVector,
                                       hashType)
      case locktime: LockTimeScriptPubKey =>
        LockTimeSpendingInfoFull(outPoint,
                                 amount,
                                 locktime,
                                 signers.toVector,
                                 hashType,
                                 conditionalPath)
      case conditional: ConditionalScriptPubKey =>
        ConditionalSpendingInfoFull(outPoint,
                                    amount,
                                    conditional,
                                    signers.toVector,
                                    hashType,
                                    conditionalPath)
      case EmptyScriptPubKey => EmptySpendingInfo(outPoint, amount, hashType)
      case _: P2SHScriptPubKey =>
        throw new IllegalArgumentException(
          "RawScriptUTXOSpendingInfo cannot contain a P2SH SPK")
      case _: NonStandardScriptPubKey | _: WitnessCommitment =>
        throw new UnsupportedOperationException(
          s"Currently unsupported ScriptPubKey $scriptPubKey")
    }
  }
}

/** For spending EmptyScriptPubKeys in tests. Probably should not be used in real life */
case class EmptySpendingInfo(
    outPoint: TransactionOutPoint,
    amount: CurrencyUnit,
    hashType: HashType)
    extends RawScriptUTXOSpendingInfo {
  override def scriptPubKey: EmptyScriptPubKey.type = EmptyScriptPubKey
  override def signer: Sign =
    throw new UnsupportedOperationException("EmptySpendingInfo has no signer.")
  override def signers: Seq[Sign] = Vector.empty
  override def conditionalPath: ConditionalPath =
    ConditionalPath.NoConditionsLeft
}

case class P2PKSpendingInfo(
    outPoint: TransactionOutPoint,
    amount: CurrencyUnit,
    scriptPubKey: P2PKScriptPubKey,
    override val signer: Sign,
    hashType: HashType)
    extends RawScriptUTXOSpendingInfo {
  require(scriptPubKey.publicKey == signer.publicKey,
          "Signer pubkey must match ScriptPubKey")

  override val signers: Vector[Sign] = Vector(signer)

  override def conditionalPath: ConditionalPath =
    ConditionalPath.NoConditionsLeft
}

case class P2PKHSpendingInfo(
    outPoint: TransactionOutPoint,
    amount: CurrencyUnit,
    scriptPubKey: P2PKHScriptPubKey,
    override val signer: Sign,
    hashType: HashType)
    extends RawScriptUTXOSpendingInfo {
  require(scriptPubKey == P2PKHScriptPubKey(signer.publicKey),
          "Signer pubkey must match ScriptPubKey")

  override val signers: Vector[Sign] = Vector(signer)

  override def conditionalPath: ConditionalPath =
    ConditionalPath.NoConditionsLeft
}

case class P2PKWithTimeoutSpendingInfo(
    outPoint: TransactionOutPoint,
    amount: CurrencyUnit,
    scriptPubKey: P2PKWithTimeoutScriptPubKey,
    override val signer: Sign,
    hashType: HashType,
    isBeforeTimeout: Boolean)
    extends RawScriptUTXOSpendingInfo {
  require(
    scriptPubKey.pubKey == signer.publicKey || scriptPubKey.timeoutPubKey == signer.publicKey,
    "Signer pubkey must match ScriptPubKey")

  override val signers: Vector[Sign] = Vector(signer)

  override def conditionalPath: ConditionalPath =
    if (isBeforeTimeout) {
      ConditionalPath.nonNestedTrue
    } else {
      ConditionalPath.nonNestedFalse
    }
}

sealed trait MultiSignatureSpendingInfo
    extends RawScriptUTXOSpendingInfoSingle {
  override def conditionalPath: ConditionalPath =
    ConditionalPath.NoConditionsLeft
  override def scriptPubKey: MultiSignatureScriptPubKey
}

case class MultiSignatureSpendingInfoSingle(
    outPoint: TransactionOutPoint,
    amount: CurrencyUnit,
    scriptPubKey: MultiSignatureScriptPubKey,
    signer: Sign,
    hashType: HashType
) extends MultiSignatureSpendingInfo

case class MultiSignatureSpendingInfoFull(
    outPoint: TransactionOutPoint,
    amount: CurrencyUnit,
    scriptPubKey: MultiSignatureScriptPubKey,
    signers: Vector[Sign],
    hashType: HashType
) extends RawScriptUTXOSpendingInfo
    with MultiSignatureSpendingInfo {
  require(signers.length >= scriptPubKey.requiredSigs, "Not enough signers!")
}

sealed trait ConditionalSpendingInfo extends RawScriptUTXOSpendingInfoSingle {
  require(conditionalPath != ConditionalPath.NoConditionsLeft,
          "Must specify True or False")

  override def scriptPubKey: ConditionalScriptPubKey

  def nestedSpendingInfo: RawScriptUTXOSpendingInfoSingle

  lazy val (condition: Boolean, nextConditionalPath: ConditionalPath) =
    conditionalPath match {
      case ConditionalPath.ConditionTrue(nextCondition) =>
        (true, nextCondition)
      case ConditionalPath.ConditionFalse(nextCondition) =>
        (false, nextCondition)
      case ConditionalPath.NoConditionsLeft =>
        throw new IllegalStateException(
          "This should be covered by invariant above")
    }
}

case class ConditionalSpendingInfoSingle(
    outPoint: TransactionOutPoint,
    amount: CurrencyUnit,
    scriptPubKey: ConditionalScriptPubKey,
    signer: Sign,
    hashType: HashType,
    conditionalPath: ConditionalPath)
    extends ConditionalSpendingInfo {
  override val nestedSpendingInfo: RawScriptUTXOSpendingInfoSingle = {
    val nestedSPK = if (condition) {
      scriptPubKey.trueSPK
    } else {
      scriptPubKey.falseSPK
    }

    RawScriptUTXOSpendingInfoSingle(outPoint,
                                    amount,
                                    nestedSPK,
                                    signer,
                                    hashType,
                                    nextConditionalPath)
  }

  private[utxo] val shouldBeFull: Boolean = {
    RawScriptUTXOSpendingInfoSingle.shouldBeFull(nestedSpendingInfo)
  }

  require(shouldBeFull, "You must use ConditionalSpendingInfoFull")
}

/** Info required for signing a [[ConditionalScriptPubKey]] */
case class ConditionalSpendingInfoFull(
    outPoint: TransactionOutPoint,
    amount: CurrencyUnit,
    scriptPubKey: ConditionalScriptPubKey,
    signers: Vector[Sign],
    hashType: HashType,
    conditionalPath: ConditionalPath)
    extends RawScriptUTXOSpendingInfo
    with ConditionalSpendingInfo {
  override val nestedSpendingInfo: RawScriptUTXOSpendingInfo = {
    val nestedSPK = if (condition) {
      scriptPubKey.trueSPK
    } else {
      scriptPubKey.falseSPK
    }

    RawScriptUTXOSpendingInfo(outPoint,
                              amount,
                              nestedSPK,
                              signers,
                              hashType,
                              nextConditionalPath)
  }
}

sealed trait LockTimeSpendingInfo extends RawScriptUTXOSpendingInfoSingle {
  override def scriptPubKey: LockTimeScriptPubKey

  def nestedSpendingInfo: RawScriptUTXOSpendingInfoSingle
}

case class LockTimeSpendingInfoSingle(
    outPoint: TransactionOutPoint,
    amount: CurrencyUnit,
    scriptPubKey: LockTimeScriptPubKey,
    signer: Sign,
    hashType: HashType,
    conditionalPath: ConditionalPath
) extends LockTimeSpendingInfo {
  override val nestedSpendingInfo: RawScriptUTXOSpendingInfoSingle = {
    RawScriptUTXOSpendingInfoSingle(outPoint,
                                    amount,
                                    scriptPubKey.nestedScriptPubKey,
                                    signer,
                                    hashType,
                                    conditionalPath)
  }

  private[utxo] val shouldBeFull: Boolean = {
    RawScriptUTXOSpendingInfoSingle.shouldBeFull(nestedSpendingInfo)
  }

  require(shouldBeFull, "You must use LockTimeSpendingInfoFull")
}

case class LockTimeSpendingInfoFull(
    outPoint: TransactionOutPoint,
    amount: CurrencyUnit,
    scriptPubKey: LockTimeScriptPubKey,
    signers: Vector[Sign],
    hashType: HashType,
    conditionalPath: ConditionalPath
) extends RawScriptUTXOSpendingInfo
    with LockTimeSpendingInfo {

  override val nestedSpendingInfo: RawScriptUTXOSpendingInfo = {
    RawScriptUTXOSpendingInfo(outPoint,
                              amount,
                              scriptPubKey.nestedScriptPubKey,
                              signers,
                              hashType,
                              conditionalPath)
  }
}

sealed trait SegwitV0NativeUTXOSpendingInfoSingle
    extends BitcoinUTXOSpendingInfoSingle {
  def scriptWitness: ScriptWitnessV0

  override val redeemScriptOpt: Option[ScriptPubKey] = None
  override val scriptWitnessOpt: Option[ScriptWitnessV0] = Some(scriptWitness)

}

object SegwitV0NativeUTXOSpendingInfoSingle {

  def apply(
      outPoint: TransactionOutPoint,
      amount: CurrencyUnit,
      scriptPubKey: WitnessScriptPubKeyV0,
      signer: Sign,
      hashType: HashType,
      scriptWitness: ScriptWitnessV0,
      conditionalPath: ConditionalPath): SegwitV0NativeUTXOSpendingInfoSingle = {
    ???
  }
}

/** This is the case where we are spending a [[org.bitcoins.core.protocol.script.WitnessScriptPubKeyV0 witness v0 script]]  */
sealed trait SegwitV0NativeUTXOSpendingInfo
    extends BitcoinUTXOSpendingInfo
    with SegwitV0NativeUTXOSpendingInfoSingle

object SegwitV0NativeUTXOSpendingInfo {

  def apply(
      outPoint: TransactionOutPoint,
      amount: CurrencyUnit,
      scriptPubKey: WitnessScriptPubKeyV0,
      signers: Seq[Sign],
      hashType: HashType,
      scriptWitness: ScriptWitnessV0,
      conditionalPath: ConditionalPath): SegwitV0NativeUTXOSpendingInfo = {
    scriptPubKey match {
      case p2wpkh: P2WPKHWitnessSPKV0 =>
        scriptWitness match {
          case witness: P2WPKHWitnessV0 =>
            P2WPKHV0SpendingInfo(outPoint,
                                 amount,
                                 p2wpkh,
                                 signers.head,
                                 hashType,
                                 witness)
          case _: ScriptWitnessV0 =>
            throw new IllegalArgumentException("Script witness must be P2WPKH")
        }
      case p2wsh: P2WSHWitnessSPKV0 =>
        scriptWitness match {
          case witness: P2WSHWitnessV0 =>
            P2WSHV0SpendingInfoFull(outPoint,
                                    amount,
                                    p2wsh,
                                    signers.toVector,
                                    hashType,
                                    witness,
                                    conditionalPath)
          case _: ScriptWitnessV0 =>
            throw new IllegalArgumentException("Script witness must be P2WSH")
        }
    }
  }
}

case class P2WPKHV0SpendingInfo(
    outPoint: TransactionOutPoint,
    amount: CurrencyUnit,
    scriptPubKey: P2WPKHWitnessSPKV0,
    override val signer: Sign,
    hashType: HashType,
    scriptWitness: P2WPKHWitnessV0)
    extends SegwitV0NativeUTXOSpendingInfo {
  require(P2WPKHWitnessSPKV0(signer.publicKey) == scriptPubKey,
          "Signer has incorrect public key")
  require(scriptWitness.pubKey == signer.publicKey,
          "Witness has incorrect public key")

  override def signers: Seq[Sign] = Vector(signer)
  override def conditionalPath: ConditionalPath =
    ConditionalPath.NoConditionsLeft
}

sealed trait P2WSHV0SpendingInfo extends SegwitV0NativeUTXOSpendingInfoSingle {
  require(
    CryptoUtil
      .sha256(scriptWitness.redeemScript.asmBytes) == scriptPubKey.scriptHash,
    "Witness has incorrect script")

  override def scriptPubKey: P2WSHWitnessSPKV0
  override def scriptWitness: P2WSHWitnessV0

  def nestedSpendingInfo: RawScriptUTXOSpendingInfoSingle
}

case class P2WSHV0SpendingInfoSingle(
    outPoint: TransactionOutPoint,
    amount: CurrencyUnit,
    scriptPubKey: P2WSHWitnessSPKV0,
    signer: Sign,
    hashType: HashType,
    scriptWitness: P2WSHWitnessV0,
    conditionalPath: ConditionalPath)
    extends P2WSHV0SpendingInfo {
  override val nestedSpendingInfo: RawScriptUTXOSpendingInfoSingle = {
    RawScriptUTXOSpendingInfoSingle(outPoint,
                                    amount,
                                    scriptWitness.redeemScript,
                                    signer,
                                    hashType,
                                    conditionalPath)
  }

  private[utxo] val shouldBeFull: Boolean = {
    RawScriptUTXOSpendingInfoSingle.shouldBeFull(nestedSpendingInfo)
  }

  require(shouldBeFull, "You must use P2WSHV0SpendingInfoFull")
}

case class P2WSHV0SpendingInfoFull(
    outPoint: TransactionOutPoint,
    amount: CurrencyUnit,
    scriptPubKey: P2WSHWitnessSPKV0,
    signers: Vector[Sign],
    hashType: HashType,
    scriptWitness: P2WSHWitnessV0,
    conditionalPath: ConditionalPath)
    extends SegwitV0NativeUTXOSpendingInfo
    with P2WSHV0SpendingInfo {

  override val nestedSpendingInfo: RawScriptUTXOSpendingInfo = {
    RawScriptUTXOSpendingInfo(outPoint,
                              amount,
                              scriptWitness.redeemScript,
                              signers,
                              hashType,
                              conditionalPath)
  }
}

/** This is the case where we are spending a [[org.bitcoins.core.protocol.script.WitnessScriptPubKeyV0 witness v0 script]]  */
case class UnassignedSegwitNativeUTXOSpendingInfo(
    outPoint: TransactionOutPoint,
    amount: CurrencyUnit,
    scriptPubKey: WitnessScriptPubKey,
    signers: Seq[Sign],
    hashType: HashType,
    scriptWitness: ScriptWitness,
    conditionalPath: ConditionalPath)
    extends BitcoinUTXOSpendingInfo {
  override val redeemScriptOpt: Option[ScriptPubKey] = None

  override val scriptWitnessOpt: Option[ScriptWitness] = Some(scriptWitness)
}

sealed trait P2SHSpendingInfoSingle extends BitcoinUTXOSpendingInfoSingle {
  require(
    P2SHScriptPubKey(redeemScript) == output.scriptPubKey,
    s"Given redeem script did not match hash in output script, " +
      s"got=${P2SHScriptPubKey(redeemScript).scriptHash.hex}, " +
      s"expected=${scriptPubKey.scriptHash.hex}"
  )

  override def scriptPubKey: P2SHScriptPubKey
  def redeemScript: ScriptPubKey
  def nestedSpendingInfo: BitcoinUTXOSpendingInfoSingle

  override val redeemScriptOpt: Option[ScriptPubKey] = Some(redeemScript)
}

sealed trait P2SHSpendingInfo
    extends BitcoinUTXOSpendingInfo
    with P2SHSpendingInfoSingle {
  override def nestedSpendingInfo: BitcoinUTXOSpendingInfo
}

sealed trait P2SHNoNestSpendingInfo extends P2SHSpendingInfoSingle {
  override def redeemScript: RawScriptPubKey
  override val scriptWitnessOpt: Option[ScriptWitnessV0] = None

  def nestedSpendingInfo: RawScriptUTXOSpendingInfoSingle
}

case class P2SHNoNestSpendingInfoSingle(
    outPoint: TransactionOutPoint,
    amount: CurrencyUnit,
    scriptPubKey: P2SHScriptPubKey,
    signer: Sign,
    hashType: HashType,
    redeemScript: RawScriptPubKey,
    conditionalPath: ConditionalPath)
    extends P2SHNoNestSpendingInfo {
  override val nestedSpendingInfo: RawScriptUTXOSpendingInfoSingle = {
    RawScriptUTXOSpendingInfoSingle(outPoint,
                                    amount,
                                    redeemScript,
                                    signer,
                                    hashType,
                                    conditionalPath)
  }

  require(RawScriptUTXOSpendingInfoSingle.shouldBeFull(nestedSpendingInfo),
          "You must use P2SHNoNestSpendingInfoFull")
}

/** This is the case were we are attempting to spend a [[org.bitcoins.core.protocol.script.P2SHScriptPubKey p2sh spk]] */
case class P2SHNoNestSpendingInfoFull(
    outPoint: TransactionOutPoint,
    amount: CurrencyUnit,
    scriptPubKey: P2SHScriptPubKey,
    signers: Seq[Sign],
    hashType: HashType,
    redeemScript: RawScriptPubKey,
    conditionalPath: ConditionalPath)
    extends P2SHSpendingInfo
    with P2SHNoNestSpendingInfo {
  override val nestedSpendingInfo: RawScriptUTXOSpendingInfo =
    RawScriptUTXOSpendingInfo(outPoint,
                              amount,
                              redeemScript,
                              signers,
                              hashType,
                              conditionalPath)
}

sealed trait P2SHNestedSegwitV0UTXOSpendingInfo extends P2SHSpendingInfoSingle {
  require(
    isValidScriptWitness(redeemScript, scriptWitness),
    s"Invalid ScriptWitness for redeem script: $scriptWitness - $redeemScript")

  override def redeemScript: WitnessScriptPubKeyV0
  def scriptWitness: ScriptWitnessV0
  def nestedSpendingInfo: SegwitV0NativeUTXOSpendingInfoSingle

  override val scriptWitnessOpt: Option[ScriptWitnessV0] = Some(scriptWitness)
}

case class P2SHNestedSegwitV0UTXOSpendingInfoSingle(
    outPoint: TransactionOutPoint,
    amount: CurrencyUnit,
    scriptPubKey: P2SHScriptPubKey,
    signer: Sign,
    hashType: HashType,
    redeemScript: WitnessScriptPubKeyV0,
    scriptWitness: ScriptWitnessV0,
    conditionalPath: ConditionalPath)
    extends P2SHNestedSegwitV0UTXOSpendingInfo {
  override val nestedSpendingInfo: SegwitV0NativeUTXOSpendingInfoSingle =
    SegwitV0NativeUTXOSpendingInfoSingle(outPoint,
                                         amount,
                                         redeemScript,
                                         signer,
                                         hashType,
                                         scriptWitness,
                                         conditionalPath)

  private val shouldBeFull: Boolean = {
    nestedSpendingInfo match {
      case p2wsh: P2WSHV0SpendingInfoSingle        => p2wsh.shouldBeFull
      case _: SegwitV0NativeUTXOSpendingInfoSingle => true
    }
  }

  require(shouldBeFull, "You must use P2SHNestedSegwitV0UTXOSpendingFull")
}

/** This is for the case we are spending a p2sh(p2w{pkh,sh}) script. This means that
  * we have nested a [[org.bitcoins.core.protocol.script.WitnessScriptPubKeyV0 witness spk]]
  * inside of a [[org.bitcoins.core.protocol.script.P2SHScriptPubKey p2sh spk]] */
case class P2SHNestedSegwitV0UTXOSpendingInfoFull(
    outPoint: TransactionOutPoint,
    amount: CurrencyUnit,
    scriptPubKey: P2SHScriptPubKey,
    signers: Seq[Sign],
    hashType: HashType,
    redeemScript: WitnessScriptPubKeyV0,
    scriptWitness: ScriptWitnessV0,
    conditionalPath: ConditionalPath)
    extends P2SHSpendingInfo
    with P2SHNestedSegwitV0UTXOSpendingInfo {
  override val nestedSpendingInfo: SegwitV0NativeUTXOSpendingInfo =
    SegwitV0NativeUTXOSpendingInfo(outPoint,
                                   amount,
                                   redeemScript,
                                   signers,
                                   hashType,
                                   scriptWitness,
                                   conditionalPath)
}
