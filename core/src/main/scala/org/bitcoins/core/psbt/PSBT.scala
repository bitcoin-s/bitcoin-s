package org.bitcoins.core.psbt

import org.bitcoins.core.crypto.{ECDigitalSignature, ECPublicKey, ExtPublicKey}
import org.bitcoins.core.hd.BIP32Path
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script.{
  EmptyScriptSignature,
  ScriptPubKey,
  ScriptSignature,
  ScriptWitness
}
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.protocol.{CompactSizeUInt, NetworkElement}
import org.bitcoins.core.psbt.GlobalPSBTRecord._
import org.bitcoins.core.psbt.PSBTGlobalKeyId._
import org.bitcoins.core.psbt.PSBTInputKeyId._
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.serializers.script.RawScriptWitnessParser
import org.bitcoins.core.util.Factory
import scodec.bits._

import scala.annotation.tailrec

case class PSBT(
    globalMap: GlobalPSBTMap,
    inputMaps: Vector[InputPSBTMap],
    outputMaps: Vector[OutputPSBTMap])
    extends NetworkElement {
  require(
    globalMap
      .getRecords(UnsignedTransactionKeyId)
      .size == 1)
  require(inputMaps.size == transaction.inputs.size)
  require(outputMaps.size == transaction.outputs.size)

  private val inputBytes: ByteVector =
    inputMaps.foldLeft(ByteVector.empty)(_ ++ _.bytes)

  private val outputBytes: ByteVector =
    outputMaps.foldLeft(ByteVector.empty)(_ ++ _.bytes)

  val bytes: ByteVector = PSBT.magicBytes ++
    globalMap.bytes ++
    inputBytes ++
    outputBytes

  def transaction: Transaction =
    globalMap
      .getRecords[UnsignedTransaction](UnsignedTransactionKeyId)
      .head
      .transaction

  def version: UInt32 = {
    val vec = globalMap.getRecords[Version](VersionKeyId)
    if (vec.isEmpty) { // If there is no version is it assumed 0
      UInt32.zero
    } else {
      vec.head.version
    }
  }

  def isFinalized: Boolean = inputMaps.forall(_.isFinalized)

  /**
    * Combiner defined by https://github.com/bitcoin/bips/blob/master/bip-0174.mediawiki#combiner
    * Takes another PSBT and adds all records that are not contained in this PSBT
    * A record's distinctness is determined by its key
    * @param other PSBT to be combined with
    * @return A PSBT with the combined data of the two PSBTs
    */
  def combinePSBT(other: PSBT): PSBT = {
    require(this.transaction.txId == other.transaction.txId,
            "Can only combine PSBTs with the same global transaction.")

    val global = this.globalMap.combine(other.globalMap)
    val inputs = this.inputMaps
      .zip(other.inputMaps)
      .map(input => input._1.combine(input._2))
    val outputs = this.outputMaps
      .zip(other.outputMaps)
      .map(output => output._1.combine(output._2))

    PSBT(global, inputs, outputs)
  }
}

object PSBT extends Factory[PSBT] {

  // The known version of PSBTs this library can process define by https://github.com/bitcoin/bips/blob/master/bip-0174.mediawiki#version-numbers
  final val knownVersions: Vector[UInt32] = Vector(UInt32.zero)

  // The magic bytes and separator defined by https://github.com/bitcoin/bips/blob/master/bip-0174.mediawiki#specification
  final val magicBytes = hex"70736274ff"

  def fromBytes(bytes: ByteVector): PSBT = {
    require(bytes.startsWith(magicBytes),
            s"A PSBT must start with the magic bytes $magicBytes, got: $bytes")

    val globalBytes = bytes.drop(magicBytes.size)

    val global: GlobalPSBTMap = GlobalPSBTMap.fromBytes(globalBytes)

    val txRecords = global
      .getRecords[UnsignedTransaction](UnsignedTransactionKeyId)

    if (txRecords.isEmpty)
      throw new IllegalArgumentException("Invalid PSBT. No global transaction")
    if (txRecords.size > 1)
      throw new IllegalArgumentException(
        s"Invalid PSBT. There can only be one global transaction, got: ${txRecords.size}")

    val tx = txRecords.head.transaction
    val inputBytes = globalBytes.drop(global.bytes.size)

    @tailrec
    def inputLoop(
        bytes: ByteVector,
        numInputs: Int,
        accum: Seq[InputPSBTMap]): Vector[InputPSBTMap] = {
      if (numInputs <= 0 || bytes.isEmpty) {
        accum.toVector
      } else {
        val map = InputPSBTMap.fromBytes(bytes)
        inputLoop(bytes.drop(map.bytes.size), numInputs - 1, accum :+ map)
      }
    }

    val inputMaps = inputLoop(inputBytes, tx.inputs.size, Nil)

    val outputBytes =
      inputBytes.drop(inputMaps.foldLeft(0)(_ + _.bytes.size.toInt))
    @tailrec
    def outputLoop(
        bytes: ByteVector,
        numOutputs: Int,
        accum: Seq[OutputPSBTMap]): Vector[OutputPSBTMap] = {
      if (numOutputs <= 0 || bytes.isEmpty) {
        accum.toVector
      } else {
        val map = OutputPSBTMap.fromBytes(bytes)
        outputLoop(bytes.drop(map.bytes.size), numOutputs - 1, accum :+ map)
      }
    }

    val outputMaps = outputLoop(outputBytes, tx.outputs.size, Nil)

    val remainingBytes =
      outputBytes.drop(outputMaps.foldLeft(0)(_ + _.bytes.size.toInt))

    require(remainingBytes.isEmpty,
            s"The PSBT should be empty now, got: $remainingBytes")

    PSBT(global, inputMaps, outputMaps)
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

sealed trait PSBTRecord extends NetworkElement {

  def key: ByteVector

  def value: ByteVector

  def bytes: ByteVector = {
    val keySize = CompactSizeUInt.calc(key)
    val valueSize = CompactSizeUInt.calc(value)

    keySize.bytes ++ key ++ valueSize.bytes ++ value
  }
}

object PSBTRecord {

  def fromBytes(bytes: ByteVector): (ByteVector, ByteVector) = {
    val keyCmpctUInt = CompactSizeUInt.parse(bytes)

    if (keyCmpctUInt.toLong == 0) {
      (ByteVector.empty, ByteVector.empty)
    } else {
      val key = bytes.drop(keyCmpctUInt.size).take(keyCmpctUInt.toLong)
      val valueBytes = bytes.drop(keyCmpctUInt.size + keyCmpctUInt.toLong)
      val valueCmpctUInt = CompactSizeUInt.parse(valueBytes)
      val value = valueBytes
        .drop(valueCmpctUInt.size)
        .take(valueCmpctUInt.toLong)

      (key, value)
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
    override val value: ByteVector = version.bytesLE
  }

  case class Unknown(key: ByteVector, value: ByteVector)
      extends GlobalPSBTRecord

  def fromBytes(bytes: ByteVector): GlobalPSBTRecord = {
    val (key, value) = PSBTRecord.fromBytes(bytes)
    PSBTGlobalKeyId.fromByte(key.head) match {
      case UnsignedTransactionKeyId =>
        require(key.size == 1,
                s"The key must only contain the 1 byte type, got: ${key.size}")

        UnsignedTransaction(Transaction.fromBytes(value))
      case XPubKeyKeyId =>
        val xpub = ExtPublicKey.fromBytes(key.tail.take(78))
        val fingerprint = value.take(4)
        val path = BIP32Path.fromBytesLE(value.drop(4))
        XPubKey(xpub, fingerprint, path)
      case VersionKeyId =>
        require(key.size == 1,
                s"The key must only contain the 1 byte type, got: ${key.size}")

        val version = UInt32.fromBytesLE(value)

        require(PSBT.knownVersions.contains(version),
                s"Unknown version number given: $version")
        Version(version)
      case _ => GlobalPSBTRecord.Unknown(key, value)
    }
  }
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

  def fromBytes(bytes: ByteVector): InputPSBTRecord = {
    val (key, value) = PSBTRecord.fromBytes(bytes)
    PSBTInputKeyId.fromByte(key.head) match {
      case NonWitnessUTXOKeyId =>
        require(key.size == 1,
                s"The key must only contain the 1 byte type, got: ${key.size}")

        NonWitnessOrUnknownUTXO(Transaction(value))
      case WitnessUTXOKeyId =>
        require(key.size == 1,
                s"The key must only contain the 1 byte type, got: ${key.size}")

        WitnessUTXO(TransactionOutput.fromBytes(value))
      case PartialSignatureKeyId =>
        val pubKey = ECPublicKey(key.tail)
        val sig = ECDigitalSignature(value)
        PartialSignature(pubKey, sig)
      case SighashTypeKeyId =>
        require(key.size == 1,
                s"The key must only contain the 1 byte type, got: ${key.size}")

        SigHashType(HashType(value))
      case PSBTInputKeyId.RedeemScriptKeyId =>
        require(key.size == 1,
                s"The key must only contain the 1 byte type, got: ${key.size}")

        InputPSBTRecord.RedeemScript(ScriptPubKey.fromAsmBytes(value))
      case PSBTInputKeyId.WitnessScriptKeyId =>
        require(key.size == 1,
                s"The key must only contain the 1 byte type, got: ${key.size}")

        InputPSBTRecord.WitnessScript(ScriptPubKey.fromAsmBytes(value))
      case PSBTInputKeyId.BIP32DerivationPathKeyId =>
        val pubKey = ECPublicKey(key.tail)
        val fingerprint = value.take(4)
        val path = BIP32Path.fromBytesLE(value.drop(4))
        InputPSBTRecord.BIP32DerivationPath(pubKey, fingerprint, path)
      case FinalizedScriptSigKeyId =>
        require(key.size == 1,
                s"The key must only contain the 1 byte type, got: ${key.size}")

        val sig = ScriptSignature(value)
        val extra = value.drop(sig.size)
        FinalizedScriptSig(sig, extra)
      case FinalizedScriptWitnessKeyId =>
        require(key.size == 1,
                s"The key must only contain the 1 byte type, got: ${key.size}")

        FinalizedScriptWitness(RawScriptWitnessParser.read(value))
      case ProofOfReservesCommitmentKeyId =>
        ProofOfReservesCommitment(value)
      case _ =>
        InputPSBTRecord.Unknown(key, value)
    }
  }
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

  def fromBytes(bytes: ByteVector): OutputPSBTRecord = {
    val (key, value) = PSBTRecord.fromBytes(bytes)
    PSBTOutputKeyId.fromByte(key.head) match {
      case PSBTOutputKeyId.RedeemScriptKeyId =>
        require(key.size == 1,
                s"The key must only contain the 1 byte type, got: ${key.size}")

        OutputPSBTRecord.RedeemScript(ScriptPubKey.fromAsmBytes(value))
      case PSBTOutputKeyId.WitnessScriptKeyId =>
        require(key.size == 1,
                s"The key must only contain the 1 byte type, got: ${key.size}")

        OutputPSBTRecord.WitnessScript(ScriptPubKey.fromAsmBytes(value))
      case PSBTOutputKeyId.BIP32DerivationPathKeyId =>
        val pubKey = ECPublicKey(key.tail)
        val fingerprint = value.take(4)
        val path = BIP32Path.fromBytesLE(value.drop(4))

        OutputPSBTRecord.BIP32DerivationPath(pubKey, fingerprint, path)
      case _ =>
        OutputPSBTRecord.Unknown(key, value)
    }
  }
}

sealed trait PSBTMap extends NetworkElement {
  require(elements.map(_.key).groupBy(identity).values.forall(_.length == 1),
          "All keys must be unique.")

  def elements: Vector[PSBTRecord]

  def bytes: ByteVector =
    elements.foldLeft(ByteVector.empty)(_ ++ _.bytes) ++ hex"00"
}

/**
  * A PSBTKeyId refers to the first byte of a key that signifies which kind of key-value map
  * is in a given PSBTRecord
  */
sealed trait PSBTKeyId {
  def byte: Byte
}

sealed trait PSBTGlobalKeyId extends PSBTKeyId

object PSBTGlobalKeyId {

  def fromByte(byte: Byte): PSBTGlobalKeyId = byte match {
    case UnsignedTransactionKeyId.byte => UnsignedTransactionKeyId
    case XPubKeyKeyId.byte             => XPubKeyKeyId
    case VersionKeyId.byte             => VersionKeyId
    case _: Byte                       => UnknownKeyId
  }

  final case object UnsignedTransactionKeyId extends PSBTGlobalKeyId {
    override val byte: Byte = 0x00.byteValue
  }

  final case object XPubKeyKeyId extends PSBTGlobalKeyId {
    override val byte: Byte = 0x01.byteValue
  }

  final case object VersionKeyId extends PSBTGlobalKeyId {
    override val byte: Byte = 0xFB.byteValue
  }

  final case object UnknownKeyId extends PSBTGlobalKeyId {
    override val byte: Byte = Byte.MaxValue
  }
}

sealed trait PSBTInputKeyId extends PSBTKeyId

object PSBTInputKeyId {

  def fromByte(byte: Byte): PSBTInputKeyId = byte match {
    case NonWitnessUTXOKeyId.byte            => NonWitnessUTXOKeyId
    case WitnessUTXOKeyId.byte               => WitnessUTXOKeyId
    case PartialSignatureKeyId.byte          => PartialSignatureKeyId
    case SighashTypeKeyId.byte               => SighashTypeKeyId
    case RedeemScriptKeyId.byte              => RedeemScriptKeyId
    case WitnessScriptKeyId.byte             => WitnessScriptKeyId
    case BIP32DerivationPathKeyId.byte       => BIP32DerivationPathKeyId
    case FinalizedScriptSigKeyId.byte        => FinalizedScriptSigKeyId
    case FinalizedScriptWitnessKeyId.byte    => FinalizedScriptWitnessKeyId
    case ProofOfReservesCommitmentKeyId.byte => ProofOfReservesCommitmentKeyId
    case _: Byte                             => UnknownKeyId

  }

  final case object NonWitnessUTXOKeyId extends PSBTInputKeyId {
    override val byte: Byte = 0x00.byteValue
  }

  final case object WitnessUTXOKeyId extends PSBTInputKeyId {
    override val byte: Byte = 0x01.byteValue
  }

  final case object PartialSignatureKeyId extends PSBTInputKeyId {
    override val byte: Byte = 0x02.byteValue
  }

  final case object SighashTypeKeyId extends PSBTInputKeyId {
    override val byte: Byte = 0x03.byteValue
  }

  final case object RedeemScriptKeyId extends PSBTInputKeyId {
    override val byte: Byte = 0x04.byteValue
  }

  final case object WitnessScriptKeyId extends PSBTInputKeyId {
    override val byte: Byte = 0x05.byteValue
  }

  final case object BIP32DerivationPathKeyId extends PSBTInputKeyId {
    override val byte: Byte = 0x06.byteValue
  }

  final case object FinalizedScriptSigKeyId extends PSBTInputKeyId {
    override val byte: Byte = 0x07.byteValue
  }

  final case object FinalizedScriptWitnessKeyId extends PSBTInputKeyId {
    override val byte: Byte = 0x08.byteValue
  }

  final case object ProofOfReservesCommitmentKeyId extends PSBTInputKeyId {
    override val byte: Byte = 0x09.byteValue
  }

  final case object UnknownKeyId extends PSBTInputKeyId {
    override val byte: Byte = Byte.MaxValue
  }
}

sealed trait PSBTOutputKeyId extends PSBTKeyId

object PSBTOutputKeyId {

  def fromByte(byte: Byte): PSBTOutputKeyId = byte match {
    case RedeemScriptKeyId.byte        => RedeemScriptKeyId
    case WitnessScriptKeyId.byte       => WitnessScriptKeyId
    case BIP32DerivationPathKeyId.byte => BIP32DerivationPathKeyId
    case _: Byte                       => UnknownKeyId
  }

  final case object RedeemScriptKeyId extends PSBTOutputKeyId {
    override val byte: Byte = 0x00.byteValue
  }

  final case object WitnessScriptKeyId extends PSBTOutputKeyId {
    override val byte: Byte = 0x01.byteValue
  }

  final case object BIP32DerivationPathKeyId extends PSBTOutputKeyId {
    override val byte: Byte = 0x02.byteValue
  }

  final case object UnknownKeyId extends PSBTOutputKeyId {
    override val byte: Byte = Byte.MaxValue
  }
}

case class GlobalPSBTMap(elements: Vector[GlobalPSBTRecord]) extends PSBTMap {

  def getRecords[T <: GlobalPSBTRecord](key: PSBTGlobalKeyId): Vector[T] = {
    elements
      .filter(element => PSBTGlobalKeyId.fromByte(element.key.head) == key)
      .asInstanceOf[Vector[T]]
  }

  /**
   * Takes another GlobalPSBTMap and adds all records that are not contained in this GlobalPSBTMap
   * @param other GlobalPSBTMap to be combined with
   * @return A GlobalPSBTMap with the combined data of the two GlobalPSBTMaps
   */
  def combine(other: GlobalPSBTMap): GlobalPSBTMap = {
    GlobalPSBTMap((this.elements ++ other.elements).distinctBy(_.key))
  }
}

object GlobalPSBTMap {

  def fromBytes(bytes: ByteVector): GlobalPSBTMap = {
    @tailrec
    def loop(
        remainingBytes: ByteVector,
        accum: Seq[GlobalPSBTRecord]): Vector[GlobalPSBTRecord] = {
      if (remainingBytes.head == 0x00.byteValue) {
        accum.toVector
      } else {
        val record = GlobalPSBTRecord.fromBytes(remainingBytes)
        val next = remainingBytes.drop(record.bytes.size)

        loop(next, accum :+ record)
      }
    }
    GlobalPSBTMap(loop(bytes, Nil))
  }
}

case class InputPSBTMap(elements: Vector[InputPSBTRecord]) extends PSBTMap {

  def getRecords[T <: InputPSBTRecord](key: PSBTInputKeyId): Vector[T] = {
    elements
      .filter(element => PSBTInputKeyId.fromByte(element.key.head) == key)
      .asInstanceOf[Vector[T]]
  }

  def isFinalized: Boolean =
    getRecords(FinalizedScriptSigKeyId).nonEmpty || getRecords(
      FinalizedScriptWitnessKeyId).nonEmpty

  /**
   * Takes another InputPSBTMap and adds all records that are not contained in this InputPSBTMap
   * A record's distinctness is determined by its key
   * @param other InputPSBTMap to be combined with
   * @return A InputPSBTMap with the combined data of the two InputPSBTMaps
   */
  def combine(other: InputPSBTMap): InputPSBTMap = {
    InputPSBTMap((this.elements ++ other.elements).distinctBy(_.key))
  }
}

object InputPSBTMap {

  def fromBytes(bytes: ByteVector): InputPSBTMap = {
    @tailrec
    def loop(
        remainingBytes: ByteVector,
        accum: Seq[InputPSBTRecord]): Vector[InputPSBTRecord] = {
      if (remainingBytes.head == 0x00.byteValue) {
        accum.toVector
      } else {
        val record = InputPSBTRecord.fromBytes(remainingBytes)
        val next = remainingBytes.drop(record.bytes.size)

        loop(next, accum :+ record)
      }
    }

    InputPSBTMap(loop(bytes, Nil))
  }
}
case class OutputPSBTMap(elements: Vector[OutputPSBTRecord]) extends PSBTMap {

  def getRecords[T <: OutputPSBTRecord](key: PSBTOutputKeyId): Vector[T] = {
    elements
      .filter(element => PSBTOutputKeyId.fromByte(element.key.head) == key)
      .asInstanceOf[Vector[T]]
  }

  /**
   * Takes another OutputPSBTMap and adds all records that are not contained in this OutputPSBTMap
   * A record's distinctness is determined by its key
   * @param other OutputPSBTMap to be combined with
   * @return A OutputPSBTMap with the combined data of the two OutputPSBTMaps
   */
  def combine(other: OutputPSBTMap): OutputPSBTMap = {
    OutputPSBTMap((this.elements ++ other.elements).distinctBy(_.key))
  }
}

object OutputPSBTMap {

  def fromBytes(bytes: ByteVector): OutputPSBTMap = {
    @tailrec
    def loop(
        remainingBytes: ByteVector,
        accum: Seq[OutputPSBTRecord]): Vector[OutputPSBTRecord] = {
      if (remainingBytes.head == 0x00.byteValue) {
        accum.toVector
      } else {
        val record = OutputPSBTRecord.fromBytes(remainingBytes)
        val next = remainingBytes.drop(record.bytes.size)

        loop(next, accum :+ record)
      }
    }

    OutputPSBTMap(loop(bytes, Nil))
  }
}
