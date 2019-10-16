package org.bitcoins.core.psbt

import org.bitcoins.core.crypto.{ECDigitalSignature, ECPublicKey, ExtPublicKey}
import org.bitcoins.core.hd.BIP32Path
import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.protocol.script.{
  EmptyScriptSignature,
  ScriptPubKey,
  ScriptSignature,
  ScriptWitness,
  WitnessScriptPubKey
}
import org.bitcoins.core.protocol.transaction.{
  BaseTransaction,
  EmptyWitness,
  Transaction,
  WitnessTransaction
}
import org.bitcoins.core.script.crypto.HashType
import scodec.bits._

case class PSBT(
    globalMap: GlobalPSBTMap,
    inputMaps: Vector[InputPSBTMap],
    outputMaps: Vector[OutputPSBTMap]) {
  // TODO: validation that we have the right number of input and output maps

  val bytes: ByteVector = hex"70736274" ++ hex"ff" ++ globalMap.bytes ++ inputMaps
    .foldLeft(ByteVector.empty)(_ ++ _.bytes) ++ outputMaps.foldLeft(
    ByteVector.empty)(_ ++ _.bytes)
}

sealed trait PSBTRecord {

  def key: ByteVector

  def value: ByteVector

  def bytes: ByteVector = {
    val keySize = CompactSizeUInt.calc(key)
    val valueSize = CompactSizeUInt.calc(value)

    keySize.bytes ++ key ++ valueSize.bytes ++ value
  }
}

sealed trait GlobalPSBTRecord extends PSBTRecord

object GlobalPSBTRecord {
  case class UnsignedTransaction(transaction: Transaction)
      extends GlobalPSBTRecord {
    require(
      transaction.inputs.forall(_.scriptSignature == EmptyScriptSignature),
      "All ScriptSignatures must be empty")
    private val witnessIsEmpty = transaction match {
      case wtx: WitnessTransaction => wtx.witness == EmptyWitness
      case _: BaseTransaction      => true
    }
    require(witnessIsEmpty, "Witness must be empty")

    override val key: ByteVector = hex"00"
    override val value: ByteVector = transaction.bytes
  }

  case class XPubKey(
      xpub: ExtPublicKey,
      masterFingerprint: ByteVector,
      derivationPath: BIP32Path)
      extends GlobalPSBTRecord {
    require(derivationPath.path.length == xpub.depth.toInt,
            "Derivation path length does not match xpubkey depth")

    override val key: ByteVector = hex"01" ++ xpub.bytes
    override val value: ByteVector = derivationPath.path
      .foldLeft(masterFingerprint)(_ ++ _.toUInt32.bytes)
  }
}

sealed trait InputPSBTRecord extends PSBTRecord

object InputPSBTRecord {
  case class NonWitnessOrUnknownUTXO(transactionSpent: Transaction)
      extends InputPSBTRecord {
    override val key: ByteVector = hex"00"
    override val value: ByteVector = transactionSpent.bytes
  }

  case class WitnessUTXO(spentWitnessTransaction: WitnessTransaction)
      extends InputPSBTRecord {
    override val key: ByteVector = hex"01"
    override val value: ByteVector = spentWitnessTransaction.bytes
  }

  case class PartialSignature(
      pubKey: ECPublicKey,
      signature: ECDigitalSignature)
      extends InputPSBTRecord {
    override val key: ByteVector = hex"02" ++ pubKey.bytes
    override val value: ByteVector = signature.bytes
  }

  case class SigHashType(hashType: HashType) extends InputPSBTRecord {
    override val key: ByteVector = hex"03"
    override val value: ByteVector = hashType.num.bytes
  }

  case class RedeemScript(redeemScript: ScriptPubKey) extends InputPSBTRecord {
    override val key: ByteVector = hex"04"
    override val value: ByteVector = redeemScript.bytes
  }

  case class WitnessScript(witnessScript: WitnessScriptPubKey)
      extends InputPSBTRecord {
    override val key: ByteVector = hex"05"
    override val value: ByteVector = witnessScript.bytes
  }

  case class BIP32DerivationPath(
      pubKey: ECPublicKey,
      masterFingerprint: ByteVector,
      path: BIP32Path)
      extends InputPSBTRecord {
    override val key: ByteVector = hex"06" ++ pubKey.bytes
    override val value: ByteVector =
      path.path.foldLeft(masterFingerprint)(_ ++ _.toUInt32.bytes)
  }

  case class FinalizedScriptSig(scriptSig: ScriptSignature)
      extends InputPSBTRecord {
    override val key: ByteVector = hex"07"
    override val value: ByteVector = scriptSig.bytes
  }

  case class FinalizedScriptWitness(scriptWitness: ScriptWitness)
      extends InputPSBTRecord {
    override val key: ByteVector = hex"08"
    override val value: ByteVector = scriptWitness.bytes
  }

  case class ProofOfReservesCommitment(porCommitment: ByteVector)
      extends InputPSBTRecord {
    override val key: ByteVector = hex"09"
    override val value: ByteVector = porCommitment
  }
}

sealed trait OutputPSBTRecord extends PSBTRecord

object OutputPSBTRecord {
  case class RedeemScript(redeemScript: ScriptPubKey) extends OutputPSBTRecord {
    override val key: ByteVector = hex"00"
    override val value: ByteVector = redeemScript.bytes
  }

  case class WitnessScript(witnessScript: WitnessScriptPubKey)
      extends OutputPSBTRecord {
    override val key: ByteVector = hex"01"
    override val value: ByteVector = witnessScript.bytes
  }

  case class BIP32DerivationPath(
      pubKey: ECPublicKey,
      masterFingerprint: ByteVector,
      path: BIP32Path)
      extends OutputPSBTRecord {
    override val key: ByteVector = hex"02" ++ pubKey.bytes
    override val value: ByteVector =
      path.path.foldLeft(masterFingerprint)(_ ++ _.toUInt32.bytes)
  }
}

sealed trait PSBTMap {
  require(elements.map(_.key).groupBy(identity).values.forall(_.length == 1),
          "All keys must be unique.")

  def elements: Vector[PSBTRecord]

  def bytes: ByteVector = {
    elements.foldLeft(ByteVector.empty)(_ ++ _.bytes) ++ hex"00"
  }
}

case class GlobalPSBTMap(elements: Vector[GlobalPSBTRecord]) extends PSBTMap
case class InputPSBTMap(elements: Vector[InputPSBTRecord]) extends PSBTMap
case class OutputPSBTMap(elements: Vector[OutputPSBTRecord]) extends PSBTMap
