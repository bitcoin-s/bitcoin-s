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
  * Contains the information required to spend a unspent transaction output (UTXO)
  * on a blockchain.
  */
sealed trait UTXOSpendingInfo extends SpendingInfo

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

sealed trait BitcoinUTXOSpendingInfo
    extends BitcoinPartialSpendingInfo
    with UTXOSpendingInfo

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
                P2SHNestedSegwitV0UTXOSpendingInfo(
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
                P2SHNoNestUTXOSpendingInfo(outPoint,
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
        P2PKUTXOSpendingInfo(outPoint,
                             output.value,
                             p2pk,
                             signers.head,
                             hashType)
      case p2pkh: P2PKHScriptPubKey =>
        P2PKHUTXOSpendingInfo(outPoint,
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
            P2PKWithTimeoutUTXOSpendingInfo(outPoint,
                                            output.value,
                                            p2pkWithTimeout,
                                            signers.head,
                                            hashType,
                                            beforeTimeout)
        }
      case multisig: MultiSignatureScriptPubKey =>
        MultiSignatureUTXOSpendingInfo(outPoint,
                                       output.value,
                                       multisig,
                                       signers.toVector,
                                       hashType)
      case locktime: LockTimeScriptPubKey =>
        LockTimeUTXOSpendingInfo(outPoint,
                                 output.value,
                                 locktime,
                                 signers.toVector,
                                 hashType,
                                 conditionalPath)
      case conditional: ConditionalScriptPubKey =>
        ConditionalUTXOSpendingInfo(outPoint,
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

/** This represents the information needed to be spend scripts like
  * [[org.bitcoins.core.protocol.script.P2PKHScriptPubKey p2pkh]] or [[org.bitcoins.core.protocol.script.P2PKScriptPubKey p2pk]]
  * scripts. Basically there is no nesting that requires a redeem script here*/
sealed trait RawScriptUTXOSpendingInfo
    extends RawScriptPartialSpendingInfo
    with BitcoinUTXOSpendingInfo

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
        P2PKUTXOSpendingInfo(outPoint, amount, p2pk, signers.head, hashType)
      case p2pkh: P2PKHScriptPubKey =>
        P2PKHUTXOSpendingInfo(outPoint, amount, p2pkh, signers.head, hashType)
      case p2pkWithTimeout: P2PKWithTimeoutScriptPubKey =>
        conditionalPath.headOption match {
          case None =>
            throw new IllegalArgumentException(
              "ConditionalPath must be specified for P2PKWithTimeout")
          case Some(beforeTimeout) =>
            P2PKWithTimeoutUTXOSpendingInfo(outPoint,
                                            amount,
                                            p2pkWithTimeout,
                                            signers.head,
                                            hashType,
                                            beforeTimeout)
        }
      case multisig: MultiSignatureScriptPubKey =>
        MultiSignatureUTXOSpendingInfo(outPoint,
                                       amount,
                                       multisig,
                                       signers.toVector,
                                       hashType)
      case locktime: LockTimeScriptPubKey =>
        LockTimeUTXOSpendingInfo(outPoint,
                                 amount,
                                 locktime,
                                 signers.toVector,
                                 hashType,
                                 conditionalPath)
      case conditional: ConditionalScriptPubKey =>
        ConditionalUTXOSpendingInfo(outPoint,
                                    amount,
                                    conditional,
                                    signers.toVector,
                                    hashType,
                                    conditionalPath)
      case EmptyScriptPubKey =>
        EmptyUTXOSpendingInfo(outPoint, amount, hashType)
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
case class EmptyUTXOSpendingInfo(
    outPoint: TransactionOutPoint,
    amount: CurrencyUnit,
    hashType: HashType)
    extends EmptySpendingInfo
    with RawScriptUTXOSpendingInfo

case class P2PKUTXOSpendingInfo(
    outPoint: TransactionOutPoint,
    amount: CurrencyUnit,
    scriptPubKey: P2PKScriptPubKey,
    signer: Sign,
    hashType: HashType)
    extends P2PKSpendingInfo
    with RawScriptUTXOSpendingInfo

case class P2PKHUTXOSpendingInfo(
    outPoint: TransactionOutPoint,
    amount: CurrencyUnit,
    scriptPubKey: P2PKHScriptPubKey,
    signer: Sign,
    hashType: HashType)
    extends P2PKHSpendingInfo
    with RawScriptUTXOSpendingInfo

case class P2PKWithTimeoutUTXOSpendingInfo(
    outPoint: TransactionOutPoint,
    amount: CurrencyUnit,
    scriptPubKey: P2PKWithTimeoutScriptPubKey,
    signer: Sign,
    hashType: HashType,
    isBeforeTimeout: Boolean)
    extends P2PKWithTimeoutSpendingInfo
    with RawScriptUTXOSpendingInfo

case class MultiSignatureUTXOSpendingInfo(
    outPoint: TransactionOutPoint,
    amount: CurrencyUnit,
    scriptPubKey: MultiSignatureScriptPubKey,
    signers: Vector[Sign],
    hashType: HashType)
    extends MultiSignatureSpendingInfo
    with RawScriptUTXOSpendingInfo
with
{
  require(signers.length >= scriptPubKey.requiredSigs, "Not enough signers!")
}

/** Info required for signing a [[ConditionalScriptPubKey]] */
case class ConditionalUTXOSpendingInfo(
    outPoint: TransactionOutPoint,
    amount: CurrencyUnit,
    scriptPubKey: ConditionalScriptPubKey,
    signers: Vector[Sign],
    hashType: HashType,
    conditionalPath: ConditionalPath)
    extends ConditionalSpendingInfo
    with RawScriptUTXOSpendingInfo {
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

case class LockTimeUTXOSpendingInfo(
    outPoint: TransactionOutPoint,
    amount: CurrencyUnit,
    scriptPubKey: LockTimeScriptPubKey,
    signers: Vector[Sign],
    hashType: HashType,
    conditionalPath: ConditionalPath)
    extends LockTimeSpendingInfo
    with RawScriptUTXOSpendingInfo {
  override val nestedSpendingInfo: RawScriptUTXOSpendingInfo = {
    RawScriptUTXOSpendingInfo(outPoint,
                              amount,
                              scriptPubKey.nestedScriptPubKey,
                              signers,
                              hashType,
                              conditionalPath)
  }
}

/** This is the case where we are spending a [[org.bitcoins.core.protocol.script.WitnessScriptPubKeyV0 witness v0 script]]  */
sealed trait SegwitV0NativeUTXOSpendingInfo
    extends SegwitV0NativePartialSpendingInfo
    with BitcoinUTXOSpendingInfo

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
            P2WPKHV0UTXOSpendingInfo(outPoint,
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
            P2WSHV0UTXOSpendingInfo(outPoint,
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

case class P2WPKHV0UTXOSpendingInfo(
    outPoint: TransactionOutPoint,
    amount: CurrencyUnit,
    scriptPubKey: P2WPKHWitnessSPKV0,
    signer: Sign,
    hashType: HashType,
    scriptWitness: P2WPKHWitnessV0)
    extends P2WPKHV0SpendingInfo
    with SegwitV0NativeUTXOSpendingInfo

case class P2WSHV0UTXOSpendingInfo(
    outPoint: TransactionOutPoint,
    amount: CurrencyUnit,
    scriptPubKey: P2WSHWitnessSPKV0,
    signers: Vector[Sign],
    hashType: HashType,
    scriptWitness: P2WSHWitnessV0,
    conditionalPath: ConditionalPath)
    extends P2WSHV0SpendingInfo
    with SegwitV0NativeUTXOSpendingInfo {
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
    extends UnassignedSegwitNativeSpendingInfo
    with BitcoinUTXOSpendingInfo

sealed trait P2SHUTXOSpendingInfo
    extends P2SHSpendingInfo
    with BitcoinUTXOSpendingInfo {
  override val nestedSpendingInfo: BitcoinUTXOSpendingInfo
}

/** This is the case were we are attempting to spend a [[org.bitcoins.core.protocol.script.P2SHScriptPubKey p2sh spk]] */
case class P2SHNoNestUTXOSpendingInfo(
    outPoint: TransactionOutPoint,
    amount: CurrencyUnit,
    scriptPubKey: P2SHScriptPubKey,
    signers: Seq[Sign],
    hashType: HashType,
    redeemScript: RawScriptPubKey,
    conditionalPath: ConditionalPath)
    extends P2SHNoNestSpendingInfo
    with P2SHUTXOSpendingInfo {
  override val nestedSpendingInfo: RawScriptUTXOSpendingInfo =
    RawScriptUTXOSpendingInfo(outPoint,
                              amount,
                              redeemScript,
                              signers,
                              hashType,
                              conditionalPath)
}

/** This is for the case we are spending a p2sh(p2w{pkh,sh}) script. This means that
  * we have nested a [[org.bitcoins.core.protocol.script.WitnessScriptPubKeyV0 witness spk]]
  * inside of a [[org.bitcoins.core.protocol.script.P2SHScriptPubKey p2sh spk]] */
case class P2SHNestedSegwitV0UTXOSpendingInfo(
    outPoint: TransactionOutPoint,
    amount: CurrencyUnit,
    scriptPubKey: P2SHScriptPubKey,
    signers: Seq[Sign],
    hashType: HashType,
    redeemScript: WitnessScriptPubKeyV0,
    scriptWitness: ScriptWitnessV0,
    conditionalPath: ConditionalPath)
    extends P2SHNestedSegwitV0SpendingInfo
    with P2SHUTXOSpendingInfo {
  override val nestedSpendingInfo: SegwitV0NativeUTXOSpendingInfo =
    SegwitV0NativeUTXOSpendingInfo(outPoint,
                                   amount,
                                   redeemScript,
                                   signers,
                                   hashType,
                                   scriptWitness,
                                   conditionalPath)
}
