package org.bitcoins.core.wallet.utxo

import org.bitcoins.core.crypto.Sign
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.protocol.script.{
  EmptyScriptPubKey,
  EmptyScriptWitness,
  LockTimeScriptPubKey,
  MultiSignatureScriptPubKey,
  NonStandardScriptPubKey,
  P2PKHScriptPubKey,
  P2PKScriptPubKey,
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
  * Contains the information required to spend a unspent transaction output (UTXO)
  * on a blockchain.
  */
sealed abstract class UTXOSpendingInfo {

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
}

sealed trait BitcoinUTXOSpendingInfo extends UTXOSpendingInfo {

  protected def isValidScriptWitness(
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

object BitcoinUTXOSpendingInfo {

  def apply(
      outPoint: TransactionOutPoint,
      output: TransactionOutput,
      signers: Seq[Sign],
      redeemScriptOpt: Option[ScriptPubKey],
      scriptWitnessOpt: Option[ScriptWitness],
      hashType: HashType): BitcoinUTXOSpendingInfo = {
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
                    "Script Witness must be defined for (nested) Segwit input"))
                )
              case nonWitnessSPK: RawScriptPubKey =>
                P2SHNoNestSpendingInfo(outPoint,
                                       output.value,
                                       p2sh,
                                       signers,
                                       hashType,
                                       nonWitnessSPK)
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
              "Script Witness must be defined for Segwit input"))
        )
      case wspk: UnassignedWitnessScriptPubKey =>
        UnassignedSegwitNativeUTXOSpendingInfo(
          outPoint,
          output.value,
          wspk,
          signers,
          hashType,
          scriptWitnessOpt.getOrElse(EmptyScriptWitness))
      case p2pk: P2PKScriptPubKey =>
        P2PKSpendingInfo(outPoint, output.value, p2pk, signers.head, hashType)
      case p2pkh: P2PKHScriptPubKey =>
        P2PKHSpendingInfo(outPoint, output.value, p2pkh, signers.head, hashType)
      case multisig: MultiSignatureScriptPubKey =>
        MultiSignatureSpendingInfo(outPoint,
                                   output.value,
                                   multisig,
                                   signers.toVector,
                                   hashType)
      case locktime: LockTimeScriptPubKey =>
        LockTimeSpendingInfo(outPoint,
                             output.value,
                             locktime,
                             signers.toVector,
                             hashType)
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
        HashType)] = {
    Some(info.outPoint,
         info.output,
         info.signers,
         info.redeemScriptOpt,
         info.scriptWitnessOpt,
         info.hashType)
  }
}

/** This represents the information needed to be spend scripts like
  * [[org.bitcoins.core.protocol.script.P2PKHScriptPubKey p2pkh]] or [[org.bitcoins.core.protocol.script.P2PKScriptPubKey p2pk]]
  * scripts. Basically there is no nesting that requires a redeem script here*/
sealed trait RawScriptUTXOSpendingInfo extends BitcoinUTXOSpendingInfo {
  override def outPoint: TransactionOutPoint
  override def amount: CurrencyUnit
  override def scriptPubKey: RawScriptPubKey
  override def signers: Seq[Sign]
  override def hashType: HashType

  override val redeemScriptOpt: Option[ScriptPubKey] = None

  override val scriptWitnessOpt: Option[ScriptWitnessV0] = None
}

object RawScriptUTXOSpendingInfo {

  def apply(
      outPoint: TransactionOutPoint,
      amount: CurrencyUnit,
      scriptPubKey: RawScriptPubKey,
      signers: Seq[Sign],
      hashType: HashType): RawScriptUTXOSpendingInfo = {
    scriptPubKey match {
      case p2pk: P2PKScriptPubKey =>
        P2PKSpendingInfo(outPoint, amount, p2pk, signers.head, hashType)
      case p2pkh: P2PKHScriptPubKey =>
        P2PKHSpendingInfo(outPoint, amount, p2pkh, signers.head, hashType)
      case multisig: MultiSignatureScriptPubKey =>
        MultiSignatureSpendingInfo(outPoint,
                                   amount,
                                   multisig,
                                   signers.toVector,
                                   hashType)
      case locktime: LockTimeScriptPubKey =>
        LockTimeSpendingInfo(outPoint,
                             amount,
                             locktime,
                             signers.toVector,
                             hashType)
      case _: P2SHScriptPubKey =>
        throw new IllegalArgumentException(
          "RawScriptUTXOSpendingInfo cannot contain a P2SH SPK")
      case _: NonStandardScriptPubKey | _: WitnessCommitment |
          EmptyScriptPubKey =>
        throw new UnsupportedOperationException(
          s"Currently unsupported ScriptPubKey $scriptPubKey")
    }
  }
}

case class P2PKSpendingInfo(
    outPoint: TransactionOutPoint,
    amount: CurrencyUnit,
    scriptPubKey: P2PKScriptPubKey,
    signer: Sign,
    hashType: HashType)
    extends RawScriptUTXOSpendingInfo {
  require(scriptPubKey.publicKey == signer.publicKey,
          "Signer pubkey must match ScriptPubKey")

  override val signers: Vector[Sign] = Vector(signer)
}

case class P2PKHSpendingInfo(
    outPoint: TransactionOutPoint,
    amount: CurrencyUnit,
    scriptPubKey: P2PKHScriptPubKey,
    signer: Sign,
    hashType: HashType)
    extends RawScriptUTXOSpendingInfo {
  require(scriptPubKey == P2PKHScriptPubKey(signer.publicKey),
          "Signer pubkey must match ScriptPubKey")

  override val signers: Vector[Sign] = Vector(signer)
}

case class MultiSignatureSpendingInfo(
    outPoint: TransactionOutPoint,
    amount: CurrencyUnit,
    scriptPubKey: MultiSignatureScriptPubKey,
    signers: Vector[Sign],
    hashType: HashType
) extends RawScriptUTXOSpendingInfo {
  require(signers.length >= scriptPubKey.requiredSigs, "Not enough signers!")
}

case class LockTimeSpendingInfo(
    outPoint: TransactionOutPoint,
    amount: CurrencyUnit,
    scriptPubKey: LockTimeScriptPubKey,
    signers: Vector[Sign],
    hashType: HashType
) extends RawScriptUTXOSpendingInfo {

  val nestedSpendingInfo: RawScriptUTXOSpendingInfo = {
    RawScriptUTXOSpendingInfo(outPoint,
                              amount,
                              scriptPubKey.nestedScriptPubKey,
                              signers,
                              hashType)
  }
}

/** This is the case where we are spending a [[org.bitcoins.core.protocol.script.WitnessScriptPubKeyV0 witness v0 script]]  */
sealed trait SegwitV0NativeUTXOSpendingInfo extends BitcoinUTXOSpendingInfo {
  def scriptWitness: ScriptWitnessV0

  override val redeemScriptOpt: Option[ScriptPubKey] = None
  override val scriptWitnessOpt: Option[ScriptWitnessV0] = Some(scriptWitness)
}

object SegwitV0NativeUTXOSpendingInfo {

  def apply(
      outPoint: TransactionOutPoint,
      amount: CurrencyUnit,
      scriptPubKey: WitnessScriptPubKeyV0,
      signers: Seq[Sign],
      hashType: HashType,
      scriptWitness: ScriptWitnessV0): SegwitV0NativeUTXOSpendingInfo = {
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
            P2WSHV0SpendingInfo(outPoint,
                                amount,
                                p2wsh,
                                signers.toVector,
                                hashType,
                                witness)
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
    signer: Sign,
    hashType: HashType,
    scriptWitness: P2WPKHWitnessV0)
    extends SegwitV0NativeUTXOSpendingInfo {
  require(P2WPKHWitnessSPKV0(signer.publicKey) == scriptPubKey,
          "Signer has incorrect public key")
  require(scriptWitness.pubKey == signer.publicKey,
          "Witness has incorrect public key")

  override def signers: Seq[Sign] = Vector(signer)
}

case class P2WSHV0SpendingInfo(
    outPoint: TransactionOutPoint,
    amount: CurrencyUnit,
    scriptPubKey: P2WSHWitnessSPKV0,
    signers: Vector[Sign],
    hashType: HashType,
    scriptWitness: P2WSHWitnessV0)
    extends SegwitV0NativeUTXOSpendingInfo {
  require(
    CryptoUtil
      .sha256(scriptWitness.redeemScript.asmBytes) == scriptPubKey.scriptHash,
    "Witness has incorrect script")

  val nestedSpendingInfo: RawScriptUTXOSpendingInfo = {
    RawScriptUTXOSpendingInfo(outPoint,
                              amount,
                              scriptWitness.redeemScript,
                              signers,
                              hashType)
  }
}

/** This is the case where we are spending a [[org.bitcoins.core.protocol.script.WitnessScriptPubKeyV0 witness v0 script]]  */
case class UnassignedSegwitNativeUTXOSpendingInfo(
    outPoint: TransactionOutPoint,
    amount: CurrencyUnit,
    scriptPubKey: WitnessScriptPubKey,
    signers: Seq[Sign],
    hashType: HashType,
    scriptWitness: ScriptWitness)
    extends BitcoinUTXOSpendingInfo {
  override val redeemScriptOpt: Option[ScriptPubKey] = None

  override val scriptWitnessOpt: Option[ScriptWitness] = Some(scriptWitness)
}

sealed trait P2SHSpendingInfo extends BitcoinUTXOSpendingInfo {
  override def outPoint: TransactionOutPoint
  override def amount: CurrencyUnit
  override def scriptPubKey: P2SHScriptPubKey
  override def signers: Seq[Sign]
  override def hashType: HashType
  def redeemScript: ScriptPubKey
  val nestedSpendingInfo: BitcoinUTXOSpendingInfo
}

/** This is the case were we are attempting to spend a [[org.bitcoins.core.protocol.script.P2SHScriptPubKey p2sh spk]] */
case class P2SHNoNestSpendingInfo(
    outPoint: TransactionOutPoint,
    amount: CurrencyUnit,
    scriptPubKey: P2SHScriptPubKey,
    signers: Seq[Sign],
    hashType: HashType,
    redeemScript: RawScriptPubKey)
    extends P2SHSpendingInfo {
  require(
    P2SHScriptPubKey(redeemScript) == output.scriptPubKey,
    s"Given redeem script did not match hash in output script, " +
      s"got=${P2SHScriptPubKey(redeemScript).scriptHash.hex}, " +
      s"expected=${scriptPubKey.scriptHash.hex}"
  )

  override val redeemScriptOpt: Option[ScriptPubKey] = Some(redeemScript)

  override val scriptWitnessOpt: Option[ScriptWitnessV0] = None

  override val nestedSpendingInfo: RawScriptUTXOSpendingInfo =
    RawScriptUTXOSpendingInfo(outPoint, amount, redeemScript, signers, hashType)
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
    scriptWitness: ScriptWitnessV0)
    extends P2SHSpendingInfo {
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

  override val nestedSpendingInfo: SegwitV0NativeUTXOSpendingInfo =
    SegwitV0NativeUTXOSpendingInfo(outPoint,
                                   amount,
                                   redeemScript,
                                   signers,
                                   hashType,
                                   scriptWitness)
}
