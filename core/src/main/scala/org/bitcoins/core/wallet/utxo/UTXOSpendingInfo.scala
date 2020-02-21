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

sealed abstract class UTXOSpendingInfo {

  /** The funding transaction's txid and the index of the output in the transaction we are spending */
  def outPoint: TransactionOutPoint

  def amount: CurrencyUnit

  def scriptPubKey: ScriptPubKey

  /** the actual output itself we are spending */
  def output: TransactionOutput = {
    TransactionOutput(value = amount, scriptPubKey = scriptPubKey)
  }

  /** The signer signing in the output above */
  def signers: Vector[Sign]

  def hashType: HashType

  def redeemScriptOpt: Option[ScriptPubKey]

  def scriptWitnessOpt: Option[ScriptWitness]

  def conditionalPath: ConditionalPath
}

/**
  * Contains the information required to sign an unspent transaction output (UTXO) for a single key
  */
sealed trait UTXOSpendingInfoSingle extends UTXOSpendingInfo {

  /** The signer signing in the output above */
  def signer: Sign

  override def signers: Vector[Sign] = Vector(signer)
}

/**
  * Contains the information required to fully sign an unspent transaction output (UTXO)
  * on a blockchain.
  *
  * If you want to partially sign a UTXO you will likely need to use UTXOSpendingInfoSingle.
  */
sealed trait UTXOSpendingInfoFull extends UTXOSpendingInfo {
  def requiredSigs: Int

  def toSingle(signerIndex: Int): UTXOSpendingInfoSingle

  /** Generates a UTXOSpendingInfoSingle for every Sign required to spend this
    * UTXO. Note that if more keys than necessary are specified, only the first
    * requiredSigs specified will be taken here
    */
  def toSingles: Vector[UTXOSpendingInfoSingle] = {
    signers.indices.take(requiredSigs).toVector.map(toSingle)
  }
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

  def fromBranch(branch: Vector[Boolean]): ConditionalPath = {
    if (branch.isEmpty) {
      NoConditionsLeft
    } else {
      if (branch.head) {
        ConditionTrue(fromBranch(branch.tail))
      } else {
        ConditionFalse(fromBranch(branch.tail))
      }
    }
  }
}

sealed trait BitcoinUTXOSpendingInfo extends UTXOSpendingInfo {
  protected def isValidScriptWitness(
      spk: WitnessScriptPubKeyV0,
      scriptWitness: ScriptWitnessV0): Boolean = {
    BitcoinUTXOSpendingInfo.isValidScriptWitness(spk, scriptWitness)
  }
}

object BitcoinUTXOSpendingInfo {

  def isValidScriptWitness(
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

sealed trait BitcoinUTXOSpendingInfoSingle
    extends UTXOSpendingInfoSingle
    with BitcoinUTXOSpendingInfo

object BitcoinUTXOSpendingInfoSingle {

  def apply(
      outPoint: TransactionOutPoint,
      output: TransactionOutput,
      signer: Sign,
      redeemScriptOpt: Option[ScriptPubKey],
      scriptWitnessOpt: Option[ScriptWitness],
      hashType: HashType,
      conditionalPath: ConditionalPath): BitcoinUTXOSpendingInfoSingle = {
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
                P2SHNestedSegwitV0UTXOSpendingInfoSingle(
                  outPoint,
                  output.value,
                  p2sh,
                  signer,
                  hashType,
                  wspk,
                  witnessOpt.getOrElse(throw new IllegalArgumentException(
                    "Script Witness must be defined for (nested) Segwit input")),
                  conditionalPath
                )
              case nonWitnessSPK: RawScriptPubKey =>
                P2SHNoNestSpendingInfoSingle(outPoint,
                                             output.value,
                                             p2sh,
                                             signer,
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

        SegwitV0NativeUTXOSpendingInfoSingle(
          outPoint,
          output.value,
          wspk,
          signer,
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
          Vector(signer),
          hashType,
          scriptWitnessOpt.getOrElse(EmptyScriptWitness),
          conditionalPath)
      case rawSPK: RawScriptPubKey =>
        RawScriptUTXOSpendingInfoSingle(outPoint,
                                        output.value,
                                        rawSPK,
                                        signer,
                                        hashType,
                                        conditionalPath)
    }
  }
}

sealed trait BitcoinUTXOSpendingInfoFull
    extends UTXOSpendingInfoFull
    with BitcoinUTXOSpendingInfo {

  override def toSingle(signerIndex: Int): BitcoinUTXOSpendingInfoSingle = {
    BitcoinUTXOSpendingInfoSingle(outPoint,
                                  output,
                                  signers(signerIndex),
                                  redeemScriptOpt,
                                  scriptWitnessOpt,
                                  hashType,
                                  conditionalPath)
  }

  override def toSingles: Vector[BitcoinUTXOSpendingInfoSingle] =
    super.toSingles.asInstanceOf[Vector[BitcoinUTXOSpendingInfoSingle]]
}

object BitcoinUTXOSpendingInfoFull {

  def apply(
      outPoint: TransactionOutPoint,
      output: TransactionOutput,
      signers: Vector[Sign],
      redeemScriptOpt: Option[ScriptPubKey],
      scriptWitnessOpt: Option[ScriptWitness],
      hashType: HashType,
      conditionalPath: ConditionalPath): BitcoinUTXOSpendingInfoFull = {
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

        SegwitV0NativeUTXOSpendingInfoFull(
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
      case rawSPK: RawScriptPubKey =>
        RawScriptUTXOSpendingInfoFull(outPoint,
                                      output.value,
                                      rawSPK,
                                      signers,
                                      hashType,
                                      conditionalPath)
    }
  }

  def apply(
      spendingInfoSingle: UTXOSpendingInfoSingle,
      signers: Vector[Sign]): BitcoinUTXOSpendingInfoFull = {
    require(signers.contains(spendingInfoSingle.signer),
            s"Signer from spendingInfoSingle missing in signers, got: $signers")
    BitcoinUTXOSpendingInfoFull(
      outPoint = spendingInfoSingle.outPoint,
      output = spendingInfoSingle.output,
      signers = signers,
      redeemScriptOpt = spendingInfoSingle.redeemScriptOpt,
      scriptWitnessOpt = spendingInfoSingle.scriptWitnessOpt,
      hashType = spendingInfoSingle.hashType,
      conditionalPath = spendingInfoSingle.conditionalPath
    )
  }

  def unapply(info: BitcoinUTXOSpendingInfoFull): Option[
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
}

sealed trait RawScriptUTXOSpendingInfo extends BitcoinUTXOSpendingInfo {
  override def scriptPubKey: RawScriptPubKey

  override val redeemScriptOpt: Option[ScriptPubKey] = None

  override val scriptWitnessOpt: Option[ScriptWitnessV0] = None
}

/** This represents the information needed to be sign, with a single key, scripts like
  * [[org.bitcoins.core.protocol.script.P2PKHScriptPubKey p2pkh]] or
  * [[org.bitcoins.core.protocol.script.MultiSignatureScriptPubKey multisig]] scripts.
  * Basically this is for ScriptPubKeys where there is no nesting that requires a redeem script
  */
sealed trait RawScriptUTXOSpendingInfoSingle
    extends BitcoinUTXOSpendingInfoSingle
    with RawScriptUTXOSpendingInfo

object RawScriptUTXOSpendingInfoSingle {

  def apply(
      outPoint: TransactionOutPoint,
      amount: CurrencyUnit,
      scriptPubKey: RawScriptPubKey,
      signer: Sign,
      hashType: HashType,
      conditionalPath: ConditionalPath): RawScriptUTXOSpendingInfoSingle = {
    scriptPubKey match {
      case multisig: MultiSignatureScriptPubKey =>
        MultiSignatureSpendingInfoSingle(outPoint,
                                         amount,
                                         multisig,
                                         signer,
                                         hashType)
      case locktime: LockTimeScriptPubKey =>
        LockTimeSpendingInfoSingle(outPoint,
                                   amount,
                                   locktime,
                                   signer,
                                   hashType,
                                   conditionalPath)
      case conditional: ConditionalScriptPubKey =>
        ConditionalSpendingInfoSingle(outPoint,
                                      amount,
                                      conditional,
                                      signer,
                                      hashType,
                                      conditionalPath)
      case p2pk: P2PKScriptPubKey =>
        P2PKSpendingInfo(outPoint, amount, p2pk, signer, hashType)
      case p2pkh: P2PKHScriptPubKey =>
        P2PKHSpendingInfo(outPoint, amount, p2pkh, signer, hashType)
      case p2pkWithTimeout: P2PKWithTimeoutScriptPubKey =>
        conditionalPath.headOption match {
          case None =>
            throw new IllegalArgumentException(
              "ConditionalPath must be specified for P2PKWithTimeout")
          case Some(beforeTimeout) =>
            P2PKWithTimeoutSpendingInfo(outPoint,
                                        amount,
                                        p2pkWithTimeout,
                                        signer,
                                        hashType,
                                        beforeTimeout)
        }
      case EmptyScriptPubKey | _: NonStandardScriptPubKey |
          _: WitnessCommitment =>
        throw new UnsupportedOperationException(
          s"Currently unsupported ScriptPubKey for single signing: $scriptPubKey")
    }
  }
}

/** This represents the information needed to be spend scripts like
  * [[org.bitcoins.core.protocol.script.P2PKHScriptPubKey p2pkh]] or [[org.bitcoins.core.protocol.script.P2PKScriptPubKey p2pk]]
  * scripts. Basically there is no nesting that requires a redeem script here*/
sealed trait RawScriptUTXOSpendingInfoFull
    extends BitcoinUTXOSpendingInfoFull
    with RawScriptUTXOSpendingInfo

object RawScriptUTXOSpendingInfoFull {

  def apply(
      outPoint: TransactionOutPoint,
      amount: CurrencyUnit,
      scriptPubKey: RawScriptPubKey,
      signers: Seq[Sign],
      hashType: HashType,
      conditionalPath: ConditionalPath): RawScriptUTXOSpendingInfoFull = {
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
    extends RawScriptUTXOSpendingInfoFull {
  override def scriptPubKey: EmptyScriptPubKey.type = EmptyScriptPubKey
  override def signers: Vector[Sign] = Vector.empty
  override def conditionalPath: ConditionalPath =
    ConditionalPath.NoConditionsLeft
  override def requiredSigs: Int = 0
}

case class P2PKSpendingInfo(
    outPoint: TransactionOutPoint,
    amount: CurrencyUnit,
    scriptPubKey: P2PKScriptPubKey,
    override val signer: Sign,
    hashType: HashType)
    extends RawScriptUTXOSpendingInfoFull
    with RawScriptUTXOSpendingInfoSingle {
  require(scriptPubKey.publicKey == signer.publicKey,
          "Signer pubkey must match ScriptPubKey")

  override val requiredSigs: Int = 1

  override def conditionalPath: ConditionalPath =
    ConditionalPath.NoConditionsLeft
}

case class P2PKHSpendingInfo(
    outPoint: TransactionOutPoint,
    amount: CurrencyUnit,
    scriptPubKey: P2PKHScriptPubKey,
    override val signer: Sign,
    hashType: HashType)
    extends RawScriptUTXOSpendingInfoFull
    with RawScriptUTXOSpendingInfoSingle {
  require(scriptPubKey == P2PKHScriptPubKey(signer.publicKey),
          "Signer pubkey must match ScriptPubKey")

  override val requiredSigs: Int = 1

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
    extends RawScriptUTXOSpendingInfoFull
    with RawScriptUTXOSpendingInfoSingle {
  require(
    scriptPubKey.pubKey == signer.publicKey || scriptPubKey.timeoutPubKey == signer.publicKey,
    "Signer pubkey must match ScriptPubKey")

  override val requiredSigs: Int = 1

  override def conditionalPath: ConditionalPath =
    if (isBeforeTimeout) {
      ConditionalPath.nonNestedTrue
    } else {
      ConditionalPath.nonNestedFalse
    }
}

sealed trait MultiSignatureSpendingInfo extends RawScriptUTXOSpendingInfo {
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
    with RawScriptUTXOSpendingInfoSingle

case class MultiSignatureSpendingInfoFull(
    outPoint: TransactionOutPoint,
    amount: CurrencyUnit,
    scriptPubKey: MultiSignatureScriptPubKey,
    private val signersWithPossibleExtra: Vector[Sign],
    hashType: HashType
) extends RawScriptUTXOSpendingInfoFull
    with MultiSignatureSpendingInfo {
  require(signersWithPossibleExtra.length >= scriptPubKey.requiredSigs,
          s"Not enough signers!: $this")

  override val requiredSigs: Int = scriptPubKey.requiredSigs

  override val signers: Vector[Sign] =
    signersWithPossibleExtra.take(requiredSigs)

  override def toSingle(signerIndex: Int): MultiSignatureSpendingInfoSingle = {
    MultiSignatureSpendingInfoSingle(outPoint,
                                     amount,
                                     scriptPubKey,
                                     signers(signerIndex),
                                     hashType)
  }

  /** @inheritdoc */
  override def toSingles: Vector[MultiSignatureSpendingInfoSingle] = {
    signers.map { signer =>
      MultiSignatureSpendingInfoSingle(outPoint,
                                       amount,
                                       scriptPubKey,
                                       signer,
                                       hashType)
    }
  }
}

sealed trait ConditionalSpendingInfo extends RawScriptUTXOSpendingInfo {
  require(conditionalPath != ConditionalPath.NoConditionsLeft,
          "Must specify True or False")

  override def scriptPubKey: ConditionalScriptPubKey

  def nestedSpendingInfo: RawScriptUTXOSpendingInfo

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
    extends ConditionalSpendingInfo
    with RawScriptUTXOSpendingInfoSingle {
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
}

/** Info required for signing a [[ConditionalScriptPubKey]] */
case class ConditionalSpendingInfoFull(
    outPoint: TransactionOutPoint,
    amount: CurrencyUnit,
    scriptPubKey: ConditionalScriptPubKey,
    private val signersWithPossibleExtra: Vector[Sign],
    hashType: HashType,
    conditionalPath: ConditionalPath)
    extends RawScriptUTXOSpendingInfoFull
    with ConditionalSpendingInfo {
  override val nestedSpendingInfo: RawScriptUTXOSpendingInfoFull = {
    val nestedSPK = if (condition) {
      scriptPubKey.trueSPK
    } else {
      scriptPubKey.falseSPK
    }

    RawScriptUTXOSpendingInfoFull(outPoint,
                                  amount,
                                  nestedSPK,
                                  signersWithPossibleExtra,
                                  hashType,
                                  nextConditionalPath)
  }

  override val signers: Vector[Sign] = nestedSpendingInfo.signers

  override val requiredSigs: Int = nestedSpendingInfo.requiredSigs
}

sealed trait LockTimeSpendingInfo extends RawScriptUTXOSpendingInfo {
  override def scriptPubKey: LockTimeScriptPubKey

  def nestedSpendingInfo: RawScriptUTXOSpendingInfo
}

case class LockTimeSpendingInfoSingle(
    outPoint: TransactionOutPoint,
    amount: CurrencyUnit,
    scriptPubKey: LockTimeScriptPubKey,
    signer: Sign,
    hashType: HashType,
    conditionalPath: ConditionalPath
) extends LockTimeSpendingInfo
    with RawScriptUTXOSpendingInfoSingle {
  override val nestedSpendingInfo: RawScriptUTXOSpendingInfoSingle = {
    RawScriptUTXOSpendingInfoSingle(outPoint,
                                    amount,
                                    scriptPubKey.nestedScriptPubKey,
                                    signer,
                                    hashType,
                                    conditionalPath)
  }
}

case class LockTimeSpendingInfoFull(
    outPoint: TransactionOutPoint,
    amount: CurrencyUnit,
    scriptPubKey: LockTimeScriptPubKey,
    private val signersWithPossibleExtra: Vector[Sign],
    hashType: HashType,
    conditionalPath: ConditionalPath
) extends RawScriptUTXOSpendingInfoFull
    with LockTimeSpendingInfo {

  override val nestedSpendingInfo: RawScriptUTXOSpendingInfoFull = {
    RawScriptUTXOSpendingInfoFull(outPoint,
                                  amount,
                                  scriptPubKey.nestedScriptPubKey,
                                  signersWithPossibleExtra,
                                  hashType,
                                  conditionalPath)
  }

  override val signers: Vector[Sign] = nestedSpendingInfo.signers

  override val requiredSigs: Int = nestedSpendingInfo.requiredSigs
}

sealed trait SegwitV0NativeUTXOSpendingInfo extends BitcoinUTXOSpendingInfo {
  def scriptWitness: ScriptWitnessV0

  override val redeemScriptOpt: Option[ScriptPubKey] = None
  override val scriptWitnessOpt: Option[ScriptWitnessV0] = Some(scriptWitness)
}

/** This is the case where we are signing a
  * [[org.bitcoins.core.protocol.script.WitnessScriptPubKeyV0 witness v0 script]]
  */
sealed trait SegwitV0NativeUTXOSpendingInfoSingle
    extends SegwitV0NativeUTXOSpendingInfo
    with BitcoinUTXOSpendingInfoSingle

object SegwitV0NativeUTXOSpendingInfoSingle {

  def apply(
      outPoint: TransactionOutPoint,
      amount: CurrencyUnit,
      scriptPubKey: WitnessScriptPubKeyV0,
      signer: Sign,
      hashType: HashType,
      scriptWitness: ScriptWitnessV0,
      conditionalPath: ConditionalPath): SegwitV0NativeUTXOSpendingInfoSingle = {
    scriptPubKey match {
      case p2wsh: P2WSHWitnessSPKV0 =>
        scriptWitness match {
          case witness: P2WSHWitnessV0 =>
            P2WSHV0SpendingInfoSingle(outPoint,
                                      amount,
                                      p2wsh,
                                      signer,
                                      hashType,
                                      witness,
                                      conditionalPath)
          case _: ScriptWitnessV0 =>
            throw new IllegalArgumentException("Script witness must be P2WSH")
        }
      case p2wpkh: P2WPKHWitnessSPKV0 =>
        scriptWitness match {
          case witness: P2WPKHWitnessV0 =>
            P2WPKHV0SpendingInfo(outPoint,
                                 amount,
                                 p2wpkh,
                                 signer,
                                 hashType,
                                 witness)
          case _: ScriptWitnessV0 =>
            throw new IllegalArgumentException("Script witness must be P2WPKH")
        }
    }
  }
}

/** This is the case where we are spending a [[org.bitcoins.core.protocol.script.WitnessScriptPubKeyV0 witness v0 script]]  */
sealed trait SegwitV0NativeUTXOSpendingInfoFull
    extends BitcoinUTXOSpendingInfoFull
    with SegwitV0NativeUTXOSpendingInfo

object SegwitV0NativeUTXOSpendingInfoFull {

  def apply(
      outPoint: TransactionOutPoint,
      amount: CurrencyUnit,
      scriptPubKey: WitnessScriptPubKeyV0,
      signers: Seq[Sign],
      hashType: HashType,
      scriptWitness: ScriptWitnessV0,
      conditionalPath: ConditionalPath): SegwitV0NativeUTXOSpendingInfoFull = {
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
    extends SegwitV0NativeUTXOSpendingInfoFull
    with SegwitV0NativeUTXOSpendingInfoSingle {
  require(P2WPKHWitnessSPKV0(signer.publicKey) == scriptPubKey,
          "Signer has incorrect public key")
  require(scriptWitness.pubKey == signer.publicKey,
          "Witness has incorrect public key")

  override val requiredSigs: Int = 1

  override def conditionalPath: ConditionalPath =
    ConditionalPath.NoConditionsLeft
}

sealed trait P2WSHV0SpendingInfo extends SegwitV0NativeUTXOSpendingInfo {
  require(
    CryptoUtil
      .sha256(scriptWitness.redeemScript.asmBytes) == scriptPubKey.scriptHash,
    s"Witness has incorrect script, got $scriptWitness")

  override def scriptPubKey: P2WSHWitnessSPKV0
  override def scriptWitness: P2WSHWitnessV0

  def nestedSpendingInfo: RawScriptUTXOSpendingInfo
}

case class P2WSHV0SpendingInfoSingle(
    outPoint: TransactionOutPoint,
    amount: CurrencyUnit,
    scriptPubKey: P2WSHWitnessSPKV0,
    signer: Sign,
    hashType: HashType,
    scriptWitness: P2WSHWitnessV0,
    conditionalPath: ConditionalPath)
    extends P2WSHV0SpendingInfo
    with SegwitV0NativeUTXOSpendingInfoSingle {
  override val nestedSpendingInfo: RawScriptUTXOSpendingInfoSingle = {
    RawScriptUTXOSpendingInfoSingle(outPoint,
                                    amount,
                                    scriptWitness.redeemScript,
                                    signer,
                                    hashType,
                                    conditionalPath)
  }
}

case class P2WSHV0SpendingInfoFull(
    outPoint: TransactionOutPoint,
    amount: CurrencyUnit,
    scriptPubKey: P2WSHWitnessSPKV0,
    private val signersWithPossibleExtra: Vector[Sign],
    hashType: HashType,
    scriptWitness: P2WSHWitnessV0,
    conditionalPath: ConditionalPath)
    extends SegwitV0NativeUTXOSpendingInfoFull
    with P2WSHV0SpendingInfo {

  override val nestedSpendingInfo: RawScriptUTXOSpendingInfoFull = {
    RawScriptUTXOSpendingInfoFull(outPoint,
                                  amount,
                                  scriptWitness.redeemScript,
                                  signersWithPossibleExtra,
                                  hashType,
                                  conditionalPath)
  }

  override val signers: Vector[Sign] = nestedSpendingInfo.signers

  override val requiredSigs: Int = nestedSpendingInfo.requiredSigs
}

/** This is the case where we are spending a [[org.bitcoins.core.protocol.script.WitnessScriptPubKeyV0 witness v0 script]]  */
case class UnassignedSegwitNativeUTXOSpendingInfo(
    outPoint: TransactionOutPoint,
    amount: CurrencyUnit,
    scriptPubKey: WitnessScriptPubKey,
    override val signers: Vector[Sign],
    hashType: HashType,
    scriptWitness: ScriptWitness,
    conditionalPath: ConditionalPath)
    extends BitcoinUTXOSpendingInfoFull
    with BitcoinUTXOSpendingInfoSingle {
  override val signer: Sign = signers.head

  override val redeemScriptOpt: Option[ScriptPubKey] = None

  override val scriptWitnessOpt: Option[ScriptWitness] = Some(scriptWitness)

  override val requiredSigs: Int = signers.length
}

sealed trait P2SHSpendingInfo extends BitcoinUTXOSpendingInfo {
  require(
    P2SHScriptPubKey(redeemScript) == output.scriptPubKey,
    s"Given redeem script did not match hash in output script, " +
      s"got=${P2SHScriptPubKey(redeemScript).scriptHash.hex}, " +
      s"expected=${scriptPubKey.scriptHash.hex}"
  )

  override def scriptPubKey: P2SHScriptPubKey
  def redeemScript: ScriptPubKey
  def nestedSpendingInfo: BitcoinUTXOSpendingInfo

  override val redeemScriptOpt: Option[ScriptPubKey] = Some(redeemScript)
}

sealed trait P2SHSpendingInfoSingle
    extends P2SHSpendingInfo
    with BitcoinUTXOSpendingInfoSingle {
  def nestedSpendingInfo: BitcoinUTXOSpendingInfoSingle
}

sealed trait P2SHSpendingInfoFull
    extends BitcoinUTXOSpendingInfoFull
    with P2SHSpendingInfo {
  override def nestedSpendingInfo: BitcoinUTXOSpendingInfoFull
}

sealed trait P2SHNoNestSpendingInfo extends P2SHSpendingInfo {
  override def redeemScript: RawScriptPubKey
  override val scriptWitnessOpt: Option[ScriptWitnessV0] = None

  def nestedSpendingInfo: RawScriptUTXOSpendingInfo
}

case class P2SHNoNestSpendingInfoSingle(
    outPoint: TransactionOutPoint,
    amount: CurrencyUnit,
    scriptPubKey: P2SHScriptPubKey,
    signer: Sign,
    hashType: HashType,
    redeemScript: RawScriptPubKey,
    conditionalPath: ConditionalPath)
    extends P2SHNoNestSpendingInfo
    with P2SHSpendingInfoSingle {
  override val nestedSpendingInfo: RawScriptUTXOSpendingInfoSingle = {
    RawScriptUTXOSpendingInfoSingle(outPoint,
                                    amount,
                                    redeemScript,
                                    signer,
                                    hashType,
                                    conditionalPath)
  }
}

/** This is the case were we are attempting to spend a [[org.bitcoins.core.protocol.script.P2SHScriptPubKey p2sh spk]] */
case class P2SHNoNestSpendingInfoFull(
    outPoint: TransactionOutPoint,
    amount: CurrencyUnit,
    scriptPubKey: P2SHScriptPubKey,
    private val signersWithPossibleExtra: Vector[Sign],
    hashType: HashType,
    redeemScript: RawScriptPubKey,
    conditionalPath: ConditionalPath)
    extends P2SHSpendingInfoFull
    with P2SHNoNestSpendingInfo {
  override val nestedSpendingInfo: RawScriptUTXOSpendingInfoFull =
    RawScriptUTXOSpendingInfoFull(outPoint,
                                  amount,
                                  redeemScript,
                                  signersWithPossibleExtra,
                                  hashType,
                                  conditionalPath)

  override val signers: Vector[Sign] = nestedSpendingInfo.signers

  override val requiredSigs: Int = nestedSpendingInfo.requiredSigs
}

sealed trait P2SHNestedSegwitV0UTXOSpendingInfo extends P2SHSpendingInfo {
  require(
    isValidScriptWitness(redeemScript, scriptWitness),
    s"Invalid ScriptWitness for redeem script: $scriptWitness - $redeemScript")

  override def redeemScript: WitnessScriptPubKeyV0
  def scriptWitness: ScriptWitnessV0
  def nestedSpendingInfo: SegwitV0NativeUTXOSpendingInfo

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
    extends P2SHNestedSegwitV0UTXOSpendingInfo
    with P2SHSpendingInfoSingle {
  override val nestedSpendingInfo: SegwitV0NativeUTXOSpendingInfoSingle =
    SegwitV0NativeUTXOSpendingInfoSingle(outPoint,
                                         amount,
                                         redeemScript,
                                         signer,
                                         hashType,
                                         scriptWitness,
                                         conditionalPath)
}

/** This is for the case we are spending a p2sh(p2w{pkh,sh}) script. This means that
  * we have nested a [[org.bitcoins.core.protocol.script.WitnessScriptPubKeyV0 witness spk]]
  * inside of a [[org.bitcoins.core.protocol.script.P2SHScriptPubKey p2sh spk]] */
case class P2SHNestedSegwitV0UTXOSpendingInfoFull(
    outPoint: TransactionOutPoint,
    amount: CurrencyUnit,
    scriptPubKey: P2SHScriptPubKey,
    private val signersWithPossibleExtra: Vector[Sign],
    hashType: HashType,
    redeemScript: WitnessScriptPubKeyV0,
    scriptWitness: ScriptWitnessV0,
    conditionalPath: ConditionalPath)
    extends P2SHSpendingInfo
    with P2SHNestedSegwitV0UTXOSpendingInfo
    with P2SHSpendingInfoFull {
  override val nestedSpendingInfo: SegwitV0NativeUTXOSpendingInfoFull =
    SegwitV0NativeUTXOSpendingInfoFull(outPoint,
                                       amount,
                                       redeemScript,
                                       signersWithPossibleExtra,
                                       hashType,
                                       scriptWitness,
                                       conditionalPath)

  override val signers: Vector[Sign] = nestedSpendingInfo.signers

  override val requiredSigs: Int = nestedSpendingInfo.requiredSigs
}
