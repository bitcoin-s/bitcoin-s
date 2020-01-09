package org.bitcoins.core.wallet.utxo

import org.bitcoins.core.crypto.Sign
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction.{
  TransactionOutPoint,
  TransactionOutput
}
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.util.CryptoUtil

/**
  * Contains the information required to sign a transaction
  */
abstract class SpendingInfo {

  /** The funding transaction's txid and the index of the output in the transaction we are spending */
  def outPoint: TransactionOutPoint

  def amount: CurrencyUnit

  def scriptPubKey: ScriptPubKey

  /** the actual output itself we are spending */
  def output: TransactionOutput = {
    TransactionOutput(value = amount, scriptPubKey = scriptPubKey)
  }

  /** the signers needed to spend from the output above */
  def signers: Seq[Sign]

  def hashType: HashType

  def redeemScriptOpt: Option[ScriptPubKey]

  def scriptWitnessOpt: Option[ScriptWitness]

  def conditionalPath: ConditionalPath
}

trait PartialSpendingInfo extends SpendingInfo
//{
//  self:UTXOSpendingInfo=>
//}

trait BitcoinSpendingInfo extends SpendingInfo {

  protected def isValidScriptWitness(
      spk: WitnessScriptPubKeyV0,
      scriptWitness: ScriptWitnessV0): Boolean =
    BitcoinPartialSpendingInfo.isValidScriptWitness(spk, scriptWitness)
}

trait BitcoinPartialSpendingInfo
    extends BitcoinSpendingInfo
    with PartialSpendingInfo

object BitcoinPartialSpendingInfo {

  def apply(
      outPoint: TransactionOutPoint,
      output: TransactionOutput,
      signers: Seq[Sign],
      redeemScriptOpt: Option[ScriptPubKey],
      scriptWitnessOpt: Option[ScriptWitness],
      hashType: HashType,
      conditionalPath: ConditionalPath): BitcoinPartialSpendingInfo = {
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
                P2SHNestedSegwitV0PartialSpendingInfo(
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
                P2SHNoNestPartialSpendingInfo(outPoint,
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

        SegwitV0NativePartialSpendingInfo(
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
        UnassignedSegwitNativePartialSpendingInfo(
          outPoint,
          output.value,
          wspk,
          signers,
          hashType,
          scriptWitnessOpt.getOrElse(EmptyScriptWitness),
          conditionalPath)
      case p2pk: P2PKScriptPubKey =>
        P2PKPartialSpendingInfo(outPoint,
                                output.value,
                                p2pk,
                                signers.head,
                                hashType)
      case p2pkh: P2PKHScriptPubKey =>
        P2PKHPartialSpendingInfo(outPoint,
                                 output.value,
                                 p2pkh,
                                 signers.head,
                                 hashType)
      case p2pkWithTimeout: P2PKWithTimeoutScriptPubKey =>
        conditionalPath.headOption match {
          case None =>
            throw new IllegalArgumentException(
              "ConditionalPath must be specified for P2PKWithTimeout")
          case Some(beforeTimeout) =>
            P2PKWithTimeoutPartialSpendingInfo(outPoint,
                                               output.value,
                                               p2pkWithTimeout,
                                               signers.head,
                                               hashType,
                                               beforeTimeout)
        }
      case multisig: MultiSignatureScriptPubKey =>
        MultiSignaturePartialSpendingInfo(outPoint,
                                          output.value,
                                          multisig,
                                          signers.toVector,
                                          hashType)
      case locktime: LockTimeScriptPubKey =>
        LockTimePartialSpendingInfo(outPoint,
                                    output.value,
                                    locktime,
                                    signers.toVector,
                                    hashType,
                                    conditionalPath)
      case conditional: ConditionalScriptPubKey =>
        ConditionalPartialSpendingInfo(outPoint,
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

  def unapply(info: BitcoinPartialSpendingInfo): Option[
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

/** This represents the information needed to be spend scripts like
  * [[org.bitcoins.core.protocol.script.P2PKHScriptPubKey p2pkh]] or [[org.bitcoins.core.protocol.script.P2PKScriptPubKey p2pk]]
  * scripts. Basically there is no nesting that requires a redeem script here*/
trait RawScriptPartialSpendingInfo extends BitcoinPartialSpendingInfo {
  override def outPoint: TransactionOutPoint
  override def amount: CurrencyUnit
  override def scriptPubKey: RawScriptPubKey
  override def signers: Seq[Sign]
  override def hashType: HashType

  override val redeemScriptOpt: Option[ScriptPubKey] = None

  override val scriptWitnessOpt: Option[ScriptWitnessV0] = None
}

object RawScriptPartialSpendingInfo {

  def apply(
      outPoint: TransactionOutPoint,
      amount: CurrencyUnit,
      scriptPubKey: RawScriptPubKey,
      signers: Seq[Sign],
      hashType: HashType,
      conditionalPath: ConditionalPath): RawScriptPartialSpendingInfo = {
    scriptPubKey match {
      case p2pk: P2PKScriptPubKey =>
        P2PKPartialSpendingInfo(outPoint, amount, p2pk, signers.head, hashType)
      case p2pkh: P2PKHScriptPubKey =>
        P2PKHPartialSpendingInfo(outPoint,
                                 amount,
                                 p2pkh,
                                 signers.head,
                                 hashType)
      case p2pkWithTimeout: P2PKWithTimeoutScriptPubKey =>
        conditionalPath.headOption match {
          case None =>
            throw new IllegalArgumentException(
              "ConditionalPath must be specified for P2PKWithTimeout")
          case Some(beforeTimeout) =>
            P2PKWithTimeoutPartialSpendingInfo(outPoint,
                                               amount,
                                               p2pkWithTimeout,
                                               signers.head,
                                               hashType,
                                               beforeTimeout)
        }
      case multisig: MultiSignatureScriptPubKey =>
        MultiSignaturePartialSpendingInfo(outPoint,
                                          amount,
                                          multisig,
                                          signers.toVector,
                                          hashType)
      case locktime: LockTimeScriptPubKey =>
        LockTimePartialSpendingInfo(outPoint,
                                    amount,
                                    locktime,
                                    signers.toVector,
                                    hashType,
                                    conditionalPath)
      case conditional: ConditionalScriptPubKey =>
        ConditionalPartialSpendingInfo(outPoint,
                                       amount,
                                       conditional,
                                       signers.toVector,
                                       hashType,
                                       conditionalPath)
      case EmptyScriptPubKey =>
        EmptyPartialSpendingInfo(outPoint, amount, hashType)
      case _: P2SHScriptPubKey =>
        throw new IllegalArgumentException(
          "RawScriptPartialSpendingInfo cannot contain a P2SH SPK")
      case _: NonStandardScriptPubKey | _: WitnessCommitment =>
        throw new UnsupportedOperationException(
          s"Currently unsupported ScriptPubKey $scriptPubKey")
    }
  }
}

/** For spending EmptyScriptPubKeys in tests. Probably should not be used in real life */
trait EmptySpendingInfo extends RawScriptPartialSpendingInfo {
  override def scriptPubKey: EmptyScriptPubKey.type = EmptyScriptPubKey
  override def signers: Seq[Sign] = Vector.empty
  override def conditionalPath: ConditionalPath =
    ConditionalPath.NoConditionsLeft
}

case class EmptyPartialSpendingInfo(
    outPoint: TransactionOutPoint,
    amount: CurrencyUnit,
    hashType: HashType)
    extends EmptySpendingInfo

trait P2PKSpendingInfo extends RawScriptPartialSpendingInfo {
  override val scriptPubKey: P2PKScriptPubKey

  val signer: Sign

  require(scriptPubKey.publicKey == signer.publicKey,
          "Signer pubkey must match ScriptPubKey")

  override val signers: Vector[Sign] = Vector(signer)

  override def conditionalPath: ConditionalPath =
    ConditionalPath.NoConditionsLeft
}

case class P2PKPartialSpendingInfo(
    outPoint: TransactionOutPoint,
    amount: CurrencyUnit,
    scriptPubKey: P2PKScriptPubKey,
    signer: Sign,
    hashType: HashType)
    extends P2PKSpendingInfo

trait P2PKHSpendingInfo extends RawScriptPartialSpendingInfo {
  override val scriptPubKey: P2PKHScriptPubKey

  val signer: Sign

  require(scriptPubKey == P2PKHScriptPubKey(signer.publicKey),
          "Signer pubkey must match ScriptPubKey")

  override val signers: Vector[Sign] = Vector(signer)

  override def conditionalPath: ConditionalPath =
    ConditionalPath.NoConditionsLeft
}

case class P2PKHPartialSpendingInfo(
    outPoint: TransactionOutPoint,
    amount: CurrencyUnit,
    scriptPubKey: P2PKHScriptPubKey,
    signer: Sign,
    hashType: HashType)
    extends P2PKHSpendingInfo

trait P2PKWithTimeoutSpendingInfo extends RawScriptPartialSpendingInfo {
  override val scriptPubKey: P2PKWithTimeoutScriptPubKey

  val signer: Sign

  require(
    scriptPubKey.pubKey == signer.publicKey || scriptPubKey.timeoutPubKey == signer.publicKey,
    "Signer pubkey must match ScriptPubKey")

  override val signers: Vector[Sign] = Vector(signer)

  val isBeforeTimeout: Boolean

  override def conditionalPath: ConditionalPath =
    if (isBeforeTimeout) {
      ConditionalPath.nonNestedTrue
    } else {
      ConditionalPath.nonNestedFalse
    }
}

case class P2PKWithTimeoutPartialSpendingInfo(
    outPoint: TransactionOutPoint,
    amount: CurrencyUnit,
    scriptPubKey: P2PKWithTimeoutScriptPubKey,
    signer: Sign,
    hashType: HashType,
    isBeforeTimeout: Boolean)
    extends P2PKWithTimeoutSpendingInfo

trait MultiSignatureSpendingInfo extends RawScriptPartialSpendingInfo {
  override val scriptPubKey: MultiSignatureScriptPubKey

  override def conditionalPath: ConditionalPath =
    ConditionalPath.NoConditionsLeft
}

case class MultiSignaturePartialSpendingInfo(
    outPoint: TransactionOutPoint,
    amount: CurrencyUnit,
    scriptPubKey: MultiSignatureScriptPubKey,
    signers: Vector[Sign],
    hashType: HashType
) extends MultiSignatureSpendingInfo

/** Info required for signing a [[ConditionalScriptPubKey]] */
trait ConditionalSpendingInfo extends RawScriptPartialSpendingInfo {
  override val scriptPubKey: ConditionalScriptPubKey

  require(conditionalPath != ConditionalPath.NoConditionsLeft,
          "Must specify True or False")

  val (condition: Boolean, nextConditionalPath: ConditionalPath) =
    conditionalPath match {
      case ConditionalPath.ConditionTrue(nextCondition) =>
        (true, nextCondition)
      case ConditionalPath.ConditionFalse(nextCondition) =>
        (false, nextCondition)
      case ConditionalPath.NoConditionsLeft =>
        throw new IllegalStateException(
          "This should be covered by invariant above")
    }

  val nestedSpendingInfo: RawScriptPartialSpendingInfo = {
    val nestedSPK = if (condition) {
      scriptPubKey.trueSPK
    } else {
      scriptPubKey.falseSPK
    }

    RawScriptPartialSpendingInfo(outPoint,
                                 amount,
                                 nestedSPK,
                                 signers,
                                 hashType,
                                 nextConditionalPath)
  }
}

case class ConditionalPartialSpendingInfo(
    outPoint: TransactionOutPoint,
    amount: CurrencyUnit,
    scriptPubKey: ConditionalScriptPubKey,
    signers: Vector[Sign],
    hashType: HashType,
    conditionalPath: ConditionalPath)
    extends ConditionalSpendingInfo

trait LockTimeSpendingInfo extends RawScriptPartialSpendingInfo {
  override val scriptPubKey: LockTimeScriptPubKey

  val nestedSpendingInfo: RawScriptPartialSpendingInfo = {
    RawScriptPartialSpendingInfo(outPoint,
                                 amount,
                                 scriptPubKey.nestedScriptPubKey,
                                 signers,
                                 hashType,
                                 conditionalPath)
  }
}

case class LockTimePartialSpendingInfo(
    outPoint: TransactionOutPoint,
    amount: CurrencyUnit,
    scriptPubKey: LockTimeScriptPubKey,
    signers: Vector[Sign],
    hashType: HashType,
    conditionalPath: ConditionalPath
) extends LockTimeSpendingInfo

/** This is the case where we are spending a [[org.bitcoins.core.protocol.script.WitnessScriptPubKeyV0 witness v0 script]]  */
trait SegwitV0NativePartialSpendingInfo extends BitcoinPartialSpendingInfo {
  def scriptWitness: ScriptWitnessV0

  override val redeemScriptOpt: Option[ScriptPubKey] = None
  override val scriptWitnessOpt: Option[ScriptWitnessV0] = Some(scriptWitness)
}

object SegwitV0NativePartialSpendingInfo {

  def apply(
      outPoint: TransactionOutPoint,
      amount: CurrencyUnit,
      scriptPubKey: WitnessScriptPubKeyV0,
      signers: Seq[Sign],
      hashType: HashType,
      scriptWitness: ScriptWitnessV0,
      conditionalPath: ConditionalPath): SegwitV0NativePartialSpendingInfo = {
    scriptPubKey match {
      case p2wpkh: P2WPKHWitnessSPKV0 =>
        scriptWitness match {
          case witness: P2WPKHWitnessV0 =>
            P2WPKHV0PartialSpendingInfo(outPoint,
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
            P2WSHV0PartialSpendingInfo(outPoint,
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

trait P2WPKHV0SpendingInfo extends SegwitV0NativePartialSpendingInfo {
  val signer: Sign
  val scriptWitness: P2WPKHWitnessV0
  require(P2WPKHWitnessSPKV0(signer.publicKey) == scriptPubKey,
          "Signer has incorrect public key")
  require(scriptWitness.pubKey == signer.publicKey,
          "Witness has incorrect public key")

  override def signers: Seq[Sign] = Vector(signer)
  override def conditionalPath: ConditionalPath =
    ConditionalPath.NoConditionsLeft
}

case class P2WPKHV0PartialSpendingInfo(
    outPoint: TransactionOutPoint,
    amount: CurrencyUnit,
    scriptPubKey: P2WPKHWitnessSPKV0,
    signer: Sign,
    hashType: HashType,
    scriptWitness: P2WPKHWitnessV0)
    extends P2WPKHV0SpendingInfo

trait P2WSHV0SpendingInfo extends SegwitV0NativePartialSpendingInfo {
  override val scriptPubKey: P2WSHWitnessSPKV0
  val scriptWitness: P2WSHWitnessV0
  val conditionalPath: ConditionalPath

  require(
    CryptoUtil
      .sha256(scriptWitness.redeemScript.asmBytes) == scriptPubKey.scriptHash,
    "Witness has incorrect script")

  val nestedSpendingInfo: RawScriptPartialSpendingInfo = {
    RawScriptPartialSpendingInfo(outPoint,
                                 amount,
                                 scriptWitness.redeemScript,
                                 signers,
                                 hashType,
                                 conditionalPath)
  }
}

case class P2WSHV0PartialSpendingInfo(
    outPoint: TransactionOutPoint,
    amount: CurrencyUnit,
    scriptPubKey: P2WSHWitnessSPKV0,
    signers: Vector[Sign],
    hashType: HashType,
    scriptWitness: P2WSHWitnessV0,
    conditionalPath: ConditionalPath)
    extends P2WSHV0SpendingInfo

trait UnassignedSegwitNativeSpendingInfo extends BitcoinPartialSpendingInfo {
  val scriptWitness: ScriptWitness

  val conditionalPath: ConditionalPath

  override val redeemScriptOpt: Option[ScriptPubKey] = None

  override val scriptWitnessOpt: Option[ScriptWitness] = Some(scriptWitness)
}

case class UnassignedSegwitNativePartialSpendingInfo(
    outPoint: TransactionOutPoint,
    amount: CurrencyUnit,
    scriptPubKey: WitnessScriptPubKey,
    signers: Seq[Sign],
    hashType: HashType,
    scriptWitness: ScriptWitness,
    conditionalPath: ConditionalPath)
    extends UnassignedSegwitNativeSpendingInfo

trait P2SHSpendingInfo extends BitcoinPartialSpendingInfo {
  override def outPoint: TransactionOutPoint
  override def amount: CurrencyUnit
  override def scriptPubKey: P2SHScriptPubKey
  override def signers: Seq[Sign]
  override def hashType: HashType
  def redeemScript: ScriptPubKey
  val nestedSpendingInfo: BitcoinPartialSpendingInfo
}

trait P2SHPartialSpendingInfo extends P2SHSpendingInfo

/** This is the case were we are attempting to spend a [[org.bitcoins.core.protocol.script.P2SHScriptPubKey p2sh spk]] */
trait P2SHNoNestSpendingInfo extends P2SHSpendingInfo {
  override val redeemScript: RawScriptPubKey
  require(
    P2SHScriptPubKey(redeemScript) == output.scriptPubKey,
    s"Given redeem script did not match hash in output script, " +
      s"got=${P2SHScriptPubKey(redeemScript).scriptHash.hex}, " +
      s"expected=${scriptPubKey.scriptHash.hex}"
  )

  override val redeemScriptOpt: Option[ScriptPubKey] = Some(redeemScript)

  override val scriptWitnessOpt: Option[ScriptWitnessV0] = None

  override val nestedSpendingInfo: RawScriptPartialSpendingInfo =
    RawScriptPartialSpendingInfo(outPoint,
                                 amount,
                                 redeemScript,
                                 signers,
                                 hashType,
                                 conditionalPath)
}

case class P2SHNoNestPartialSpendingInfo(
    outPoint: TransactionOutPoint,
    amount: CurrencyUnit,
    scriptPubKey: P2SHScriptPubKey,
    signers: Seq[Sign],
    hashType: HashType,
    redeemScript: RawScriptPubKey,
    conditionalPath: ConditionalPath)
    extends P2SHNoNestSpendingInfo with P2SHPartialSpendingInfo

/** This is for the case we are spending a p2sh(p2w{pkh,sh}) script. This means that
  * we have nested a [[org.bitcoins.core.protocol.script.WitnessScriptPubKeyV0 witness spk]]
  * inside of a [[org.bitcoins.core.protocol.script.P2SHScriptPubKey p2sh spk]] */
trait P2SHNestedSegwitV0SpendingInfo extends P2SHSpendingInfo {
  override val scriptPubKey: P2SHScriptPubKey

  override val redeemScript: WitnessScriptPubKeyV0

  val scriptWitness: ScriptWitnessV0

  val conditionalPath: ConditionalPath

  require(
    P2SHScriptPubKey(redeemScript) == output.scriptPubKey,
    s"Given redeem script did not match hash in output script, " +
      s"got=${P2SHScriptPubKey(redeemScript).scriptHash.hex}, " +
      s"expected=${scriptPubKey.scriptHash.hex}"
  )
  require(
    isValidScriptWitness(redeemScript, scriptWitness),
    s"Invalid ScriptWitness for redeem script: $scriptWitness - $redeemScript")

  override val redeemScriptOpt: Option[ScriptPubKey] = Some(redeemScript)

  override val scriptWitnessOpt: Option[ScriptWitnessV0] = Some(scriptWitness)

  override val nestedSpendingInfo: SegwitV0NativePartialSpendingInfo =
    SegwitV0NativePartialSpendingInfo(outPoint,
                                      amount,
                                      redeemScript,
                                      signers,
                                      hashType,
                                      scriptWitness,
                                      conditionalPath)
}

case class P2SHNestedSegwitV0PartialSpendingInfo(
    outPoint: TransactionOutPoint,
    amount: CurrencyUnit,
    scriptPubKey: P2SHScriptPubKey,
    signers: Seq[Sign],
    hashType: HashType,
    redeemScript: WitnessScriptPubKeyV0,
    scriptWitness: ScriptWitnessV0,
    conditionalPath: ConditionalPath)
    extends P2SHNestedSegwitV0SpendingInfo with P2SHPartialSpendingInfo
