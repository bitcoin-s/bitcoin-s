package org.bitcoins.core.psbt

import org.bitcoins.core.crypto.{ECDigitalSignature, ECPublicKey, ExtPublicKey}
import org.bitcoins.core.hd.BIP32Path
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.{CompactSizeUInt, NetworkElement}
import org.bitcoins.core.protocol.script.{EmptyScriptSignature, ScriptPubKey, ScriptSignature, ScriptWitness}
import org.bitcoins.core.protocol.transaction.{BaseTransaction, EmptyWitness, Transaction, TransactionOutput, WitnessTransaction}
import org.bitcoins.core.psbt.GlobalPSBTRecord.{UnsignedTransaction, Version}
import org.bitcoins.core.psbt.PSBTGlobalKey.{UnsignedTransactionKey, VersionKey}
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.util.Factory
import scodec.bits._

case class PSBT(
    globalMap: GlobalPSBTMap,
    inputMaps: Vector[InputPSBTMap],
    outputMaps: Vector[OutputPSBTMap],
    tx: Transaction)
    extends NetworkElement {
  require(inputMaps.size == tx.inputs.size)
  require(outputMaps.size == tx.outputs.size)

  private val inputBytes: ByteVector =
    inputMaps.foldLeft(ByteVector.empty)(_ ++ _.bytes)

  private val outputBytes: ByteVector =
    outputMaps.foldLeft(ByteVector.empty)(_ ++ _.bytes)

  val bytes: ByteVector = PSBT.magicBytes ++
    globalMap.bytes ++
    inputBytes ++
    outputBytes

  def version: UInt32 = {
    val vec = globalMap.getRecords[Version](VersionKey)
    if (vec.isEmpty){
      UInt32.zero
    } else {
      vec.head.version
    }
  }
}

sealed trait PSBTRecord extends NetworkElement {

  def key: ByteVector

  def value: ByteVector

  def bytes: ByteVector = {
    val keySize = CompactSizeUInt.calc(key)
    val valueSize = CompactSizeUInt.calc(value)

    keySize.bytes ++ key ++ valueSize.bytes ++ value
  }
}

object PSBT extends Factory[PSBT] {
  // The magic bytes and separator defined by https://github.com/bitcoin/bips/blob/master/bip-0174.mediawiki#specification
  final val magicBytes = hex"70736274ff"

  def fromBytes(byteVector: ByteVector): PSBT = {
    require(
      byteVector.startsWith(magicBytes),
      s"A PSBT must start with the magic bytes $magicBytes, got: $byteVector")

    val bytes = byteVector.drop(magicBytes.size)

    val globalResult: PSBTParseResult[GlobalPSBTMap] =
      PSBTHelper.parseGlobalMap(bytes)

    require(globalResult.maps.size == 1, "There should only be one global map")

    val txRecords = globalResult.maps.head
      .getRecords[UnsignedTransaction](UnsignedTransactionKey)

    if (txRecords.isEmpty)
      throw new IllegalArgumentException("Invalid PSBT. No global transaction")
    if (txRecords.size > 1)
      throw new IllegalArgumentException(
        s"Invalid PSBT. There can only be one global transaction, got: ${txRecords.size}")

    val tx = txRecords.head.transaction

    val inputsResult: PSBTParseResult[InputPSBTMap] =
      PSBTHelper.parseInputMaps(globalResult.remainingBytes, tx.inputs.size)

    val outputsResult: PSBTParseResult[OutputPSBTMap] =
      PSBTHelper.parseOutputMaps(inputsResult.remainingBytes, tx.outputs.size)

    require(
      outputsResult.remainingBytes.isEmpty,
      s"The PSBT should be empty now, got: ${outputsResult.remainingBytes}")

    PSBT(globalResult.maps.head, inputsResult.maps, outputsResult.maps, tx)
  }

  def fromBase64(base64: String): PSBT = {
    ByteVector.fromBase64(base64) match {
      case None =>
        throw new IllegalArgumentException(
          s"String given was not in base64 format, got: $base64")
      case Some(bytes) => fromBytes(bytes)
    }
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
    require(
      derivationPath.path.length == xpub.depth.toInt,
      s"Derivation path length does not match xpubkey depth, difference: ${derivationPath.path.length - xpub.depth.toInt}"
    )
    require(
      masterFingerprint.length == 4,
      s"Master key fingerprints are 4 bytes long, got: $masterFingerprint")

    override val key: ByteVector = hex"01" ++ xpub.bytes
    override val value: ByteVector = derivationPath.path
      .foldLeft(masterFingerprint)(_ ++ _.toUInt32.bytesLE)
  }

  case class Version(version: UInt32) extends GlobalPSBTRecord {
    override val key: ByteVector = hex"FB"
    override val value: ByteVector = version.bytes
  }

  case class Unknown(key: ByteVector, value: ByteVector)
      extends GlobalPSBTRecord
}

sealed trait InputPSBTRecord extends PSBTRecord

object InputPSBTRecord {
  case class NonWitnessOrUnknownUTXO(transactionSpent: Transaction)
      extends InputPSBTRecord {
    override val key: ByteVector = hex"00"
    override val value: ByteVector = transactionSpent.bytes
  }

  case class WitnessUTXO(spentWitnessTransaction: TransactionOutput)
      extends InputPSBTRecord {
    override val key: ByteVector = hex"01"
    override val value: ByteVector = spentWitnessTransaction.bytes
  }

  case class PartialSignature(
      pubKey: ECPublicKey,
      signature: ECDigitalSignature)
      extends InputPSBTRecord {
    require(pubKey.size == 33, s"pubKey must be 33 bytes, got: ${pubKey.size}")

    override val key: ByteVector = hex"02" ++ pubKey.bytes
    override val value: ByteVector = signature.bytes
  }

  case class SigHashType(hashType: HashType) extends InputPSBTRecord {
    override val key: ByteVector = hex"03"
    override val value: ByteVector = hashType.num.bytes
  }

  case class RedeemScript(redeemScript: ScriptPubKey) extends InputPSBTRecord {
    override val key: ByteVector = hex"04"
    override val value: ByteVector = redeemScript.asmBytes
  }

  case class WitnessScript(witnessScript: ScriptPubKey)
      extends InputPSBTRecord {
    override val key: ByteVector = hex"05"
    override val value: ByteVector = witnessScript.asmBytes
  }

  case class BIP32DerivationPath(
      pubKey: ECPublicKey,
      masterFingerprint: ByteVector,
      path: BIP32Path)
      extends InputPSBTRecord {
    require(pubKey.size == 33, s"pubKey must be 33 bytes, got: ${pubKey.size}")

    override val key: ByteVector = hex"06" ++ pubKey.bytes
    override val value: ByteVector =
      path.path.foldLeft(masterFingerprint)(_ ++ _.toUInt32.bytesLE)
  }

  case class FinalizedScriptSig(scriptSig: ScriptSignature, extra: ByteVector)
      extends InputPSBTRecord {
    override val key: ByteVector = hex"07"
    override val value: ByteVector = scriptSig.bytes ++ extra
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

  case class Unknown(key: ByteVector, value: ByteVector) extends InputPSBTRecord
}

sealed trait OutputPSBTRecord extends PSBTRecord

object OutputPSBTRecord {

  case class RedeemScript(redeemScript: ScriptPubKey) extends OutputPSBTRecord {
    override val key: ByteVector = hex"00"
    override val value: ByteVector = redeemScript.asmBytes
  }

  case class WitnessScript(witnessScript: ScriptPubKey)
      extends OutputPSBTRecord {

    override val key: ByteVector = hex"01"
    override val value: ByteVector = witnessScript.asmBytes
  }

  case class BIP32DerivationPath(
      pubKey: ECPublicKey,
      masterFingerprint: ByteVector,
      path: BIP32Path)
      extends OutputPSBTRecord {
    require(pubKey.size == 33, s"pubKey must be 33 bytes, got: ${pubKey.size}")

    override val key: ByteVector = hex"02" ++ pubKey.bytes
    override val value: ByteVector =
      path.path.foldLeft(masterFingerprint)(_ ++ _.toUInt32.bytesLE)
  }

  case class Unknown(key: ByteVector, value: ByteVector)
      extends OutputPSBTRecord
}

sealed trait PSBTMap extends NetworkElement {
  require(elements.map(_.key).groupBy(identity).values.forall(_.length == 1),
          "All keys must be unique.")

  def elements: Vector[PSBTRecord]

  def bytes: ByteVector =
    elements.foldLeft(ByteVector.empty)(_ ++ _.bytes) ++ hex"00"
}

// Key may be confusing because this is just the first byte of the key
// Maybe change to Identifier, Byte,  or something else
sealed trait PSBTKey {
  def byte: Byte
}

sealed trait PSBTGlobalKey extends PSBTKey

object PSBTGlobalKey {

  def fromByte(byte: Byte): PSBTGlobalKey = byte match {
    case UnsignedTransactionKey.byte => UnsignedTransactionKey
    case XPubKeyKey.byte             => XPubKeyKey
    case VersionKey.byte             => VersionKey
    case _: Byte                     => UnknownKey
  }

  final case object UnsignedTransactionKey extends PSBTGlobalKey {
    override val byte: Byte = 0x00.byteValue
  }

  final case object XPubKeyKey extends PSBTGlobalKey {
    override val byte: Byte = 0x01.byteValue
  }

  final case object VersionKey extends PSBTGlobalKey {
    override val byte: Byte = 0xFB.byteValue
  }

  final case object UnknownKey extends PSBTGlobalKey {
    override val byte: Byte = Byte.MaxValue
  }
}

sealed trait PSBTInputKey extends PSBTKey

object PSBTInputKey {

  def fromByte(byte: Byte): PSBTInputKey = byte match {
    case NonWitnessUTXOKey.byte            => NonWitnessUTXOKey
    case WitnessUTXOKey.byte               => WitnessUTXOKey
    case PartialSignatureKey.byte          => PartialSignatureKey
    case SighashTypeKey.byte               => SighashTypeKey
    case RedeemScriptKey.byte              => RedeemScriptKey
    case WitnessScriptKey.byte             => WitnessScriptKey
    case BIP32DerivationPathKey.byte       => BIP32DerivationPathKey
    case FinalizedScriptSigKey.byte        => FinalizedScriptSigKey
    case FinalizedScriptWitnessKey.byte    => FinalizedScriptWitnessKey
    case ProofOfReservesCommitmentKey.byte => ProofOfReservesCommitmentKey
    case _: Byte                           => UnknownKey

  }

  final case object NonWitnessUTXOKey extends PSBTInputKey {
    override val byte: Byte = 0x00.byteValue
  }

  final case object WitnessUTXOKey extends PSBTInputKey {
    override val byte: Byte = 0x01.byteValue
  }

  final case object PartialSignatureKey extends PSBTInputKey {
    override val byte: Byte = 0x02.byteValue
  }

  final case object SighashTypeKey extends PSBTInputKey {
    override val byte: Byte = 0x03.byteValue
  }

  final case object RedeemScriptKey extends PSBTInputKey {
    override val byte: Byte = 0x04.byteValue
  }

  final case object WitnessScriptKey extends PSBTInputKey {
    override val byte: Byte = 0x05.byteValue
  }

  final case object BIP32DerivationPathKey extends PSBTInputKey {
    override val byte: Byte = 0x06.byteValue
  }

  final case object FinalizedScriptSigKey extends PSBTInputKey {
    override val byte: Byte = 0x07.byteValue
  }

  final case object FinalizedScriptWitnessKey extends PSBTInputKey {
    override val byte: Byte = 0x08.byteValue
  }

  final case object ProofOfReservesCommitmentKey extends PSBTInputKey {
    override val byte: Byte = 0x09.byteValue
  }

  final case object UnknownKey extends PSBTInputKey {
    override val byte: Byte = Byte.MaxValue
  }
}

sealed trait PSBTOutputKey extends PSBTKey

object PSBTOutputKey {

  def fromByte(byte: Byte): PSBTOutputKey = byte match {
    case RedeemScriptKey.byte        => RedeemScriptKey
    case WitnessScriptKey.byte       => WitnessScriptKey
    case BIP32DerivationPathKey.byte => BIP32DerivationPathKey
    case _: Byte                     => UnknownKey
  }

  final case object RedeemScriptKey extends PSBTOutputKey {
    override val byte: Byte = 0x00.byteValue
  }

  final case object WitnessScriptKey extends PSBTOutputKey {
    override val byte: Byte = 0x01.byteValue
  }

  final case object BIP32DerivationPathKey extends PSBTOutputKey {
    override val byte: Byte = 0x02.byteValue
  }

  final case object UnknownKey extends PSBTOutputKey {
    override val byte: Byte = Byte.MaxValue
  }
}

case class GlobalPSBTMap(elements: Vector[GlobalPSBTRecord]) extends PSBTMap {

  def getRecords[T <: GlobalPSBTRecord](key: PSBTGlobalKey): Vector[T] = {
    elements
      .filter(element => PSBTGlobalKey.fromByte(element.key.head) == key)
      .asInstanceOf[Vector[T]]
  }
}
case class InputPSBTMap(elements: Vector[InputPSBTRecord]) extends PSBTMap {

  def getRecords[T <: InputPSBTRecord](key: PSBTInputKey): Vector[T] = {
    elements
      .filter(element => PSBTInputKey.fromByte(element.key.head) == key)
      .asInstanceOf[Vector[T]]

  }
}
case class OutputPSBTMap(elements: Vector[OutputPSBTRecord]) extends PSBTMap {

  def getRecords[T <: OutputPSBTRecord](key: PSBTOutputKey): Vector[T] = {
    elements
      .filter(element => PSBTOutputKey.fromByte(element.key.head) == key)
      .asInstanceOf[Vector[T]]
  }
}
