package org.bitcoins.core.psbt

import org.bitcoins.core.crypto.{ECDigitalSignature, ECPublicKey, ExtPublicKey}
import org.bitcoins.core.hd.BIP32Path
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.protocol.script.{ScriptPubKey, ScriptSignature}
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionOutput}
import org.bitcoins.core.psbt.GlobalPSBTRecord._
import org.bitcoins.core.psbt.InputPSBTRecord._
import org.bitcoins.core.psbt.PSBTGlobalKey._
import org.bitcoins.core.psbt.PSBTInputKey._
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.serializers.script.RawScriptWitnessParser
import scodec.bits.ByteVector

import scala.annotation.tailrec

private[psbt] case class PSBTParseResult[T <: PSBTMap](
    remainingBytes: ByteVector,
    maps: Vector[T])

object PSBTHelper {

  final private val terminator: Byte = 0x00.byteValue

  private def parseKeyAndValue(
      bytes: ByteVector): (Long, ByteVector, ByteVector) = {
    val keyCmpctUInt = CompactSizeUInt.parse(bytes)

    if (keyCmpctUInt.toLong == 0) {
      (keyCmpctUInt.size, ByteVector.empty, ByteVector.empty)
    } else {
      val key = bytes.drop(keyCmpctUInt.size).take(keyCmpctUInt.toLong)
      val valueBytes = bytes.drop(keyCmpctUInt.size + keyCmpctUInt.toLong)
      val valueCmpctUInt = CompactSizeUInt.parse(valueBytes)
      val value = valueBytes
        .drop(valueCmpctUInt.size)
        .take(valueCmpctUInt.toLong)
      val totalSize = keyCmpctUInt.size + keyCmpctUInt.toLong + valueCmpctUInt.size + valueCmpctUInt
        .toLong

      (totalSize, key, value)
    }
  }

  def parseGlobalMap(bytes: ByteVector): PSBTParseResult[GlobalPSBTMap] = {
    val (remainingBytes, records) = parseGlobalMap(bytes, Nil, None)
    PSBTParseResult[GlobalPSBTMap](remainingBytes,
                                   Vector(GlobalPSBTMap(records)))
  }

  @tailrec
  private def parseGlobalMap(
      remainingBytes: ByteVector,
      accum: Seq[GlobalPSBTRecord],
      txOpt: Option[Transaction]): (ByteVector, Vector[GlobalPSBTRecord]) = {
    if (remainingBytes.head == terminator) {
      (remainingBytes.tail, accum.toVector)
    } else {
      val (totalSize, key, value) = parseKeyAndValue(remainingBytes)
      val next = remainingBytes.drop(totalSize) // Drop the key-value map

      PSBTGlobalKey.fromByte(key.head) match {
        case UnsignedTransactionKey =>
          txOpt match {
            case Some(_) =>
              throw new IllegalArgumentException(
                "Duplicate Key, unsigned tx already provided")
            case None =>
              require(
                key.size == 1,
                s"The key must only contain the 1 byte type, got: ${key.size}")

              val transaction: Transaction = Transaction.fromBytes(value)
              parseGlobalMap(next,
                             accum :+ UnsignedTransaction(transaction),
                             Some(transaction))
          }
        case XPubKeyKey =>
          val xpub = ExtPublicKey.fromBytes(key.tail.take(78))
          val fingerprint = value.take(4)
          val path = BIP32Path.fromBytesLE(value.drop(4))
          parseGlobalMap(next, accum :+ XPubKey(xpub, fingerprint, path), txOpt)
        case VersionKey =>
          require(
            key.size == 1,
            s"The key must only contain the 1 byte type, got: ${key.size}")

          val version = UInt32(value)
          parseGlobalMap(next, accum :+ Version(version), txOpt)
        case _ =>
          parseGlobalMap(next,
                         accum :+ GlobalPSBTRecord.Unknown(key, value),
                         txOpt)
      }
    }
  }

  def parseInputMaps(
      bytes: ByteVector,
      numInputs: Int): PSBTParseResult[InputPSBTMap] = {
    parseInputMaps(bytes, numInputs, Nil)
  }

  @tailrec
  private def parseInputMaps(
      remainingBytes: ByteVector,
      remainingInputs: Int,
      accum: Seq[InputPSBTMap]): PSBTParseResult[InputPSBTMap] = {
    if (remainingInputs <= 0 || remainingBytes.isEmpty) {
      PSBTParseResult[InputPSBTMap](remainingBytes, accum.toVector)
    } else {

      val (next, map) = PSBTHelper.parseInputMap(remainingBytes, Nil)

      parseInputMaps(next, remainingInputs - 1, accum :+ InputPSBTMap(map))
    }
  }

  @tailrec
  private def parseInputMap(
      remainingBytes: ByteVector,
      accum: Seq[InputPSBTRecord]): (ByteVector, Vector[InputPSBTRecord]) = {
    if (remainingBytes.head == terminator) {
      (remainingBytes.tail, accum.toVector)
    } else {
      val (totalSize, key, value) = parseKeyAndValue(remainingBytes)

      val next = remainingBytes.drop(totalSize) // Drop the key-value map

      PSBTInputKey.fromByte(key.head) match {
        case NonWitnessUTXOKey =>
          require(
            key.size == 1,
            s"The key must only contain the 1 byte type, got: ${key.size}")

          val tx = Transaction(value)
          parseInputMap(next, accum :+ NonWitnessOrUnknownUTXO(tx))
        case WitnessUTXOKey =>
          require(
            key.size == 1,
            s"The key must only contain the 1 byte type, got: ${key.size}")

          val wtx = TransactionOutput.fromBytes(value)
          parseInputMap(next, accum :+ WitnessUTXO(wtx))
        case PartialSignatureKey =>
          val pubKey = ECPublicKey(key.tail)
          val sig = ECDigitalSignature(value)
          parseInputMap(next, accum :+ PartialSignature(pubKey, sig))
        case SighashTypeKey =>
          require(
            key.size == 1,
            s"The key must only contain the 1 byte type, got: ${key.size}")

          val hashType = HashType(value)
          parseInputMap(next, accum :+ SigHashType(hashType))
        case PSBTInputKey.RedeemScriptKey =>
          require(
            key.size == 1,
            s"The key must only contain the 1 byte type, got: ${key.size}")

          val redeemScript = ScriptPubKey.fromAsmBytes(value)
          parseInputMap(next,
                        accum :+ InputPSBTRecord.RedeemScript(redeemScript))
        case PSBTInputKey.WitnessScriptKey =>
          require(
            key.size == 1,
            s"The key must only contain the 1 byte type, got: ${key.size}")

          val witnessScript = ScriptPubKey.fromAsmBytes(value)
          parseInputMap(next,
                        accum :+ InputPSBTRecord.WitnessScript(witnessScript))
        case PSBTInputKey.BIP32DerivationPathKey =>
          val pubKey = ECPublicKey(key.tail)
          val fingerprint = value.take(4)
          val path = BIP32Path.fromBytesLE(value.drop(4))
          parseInputMap(
            next,
            accum :+ InputPSBTRecord.BIP32DerivationPath(pubKey,
                                                         fingerprint,
                                                         path))
        case FinalizedScriptSigKey =>
          require(
            key.size == 1,
            s"The key must only contain the 1 byte type, got: ${key.size}")

          val sig = ScriptSignature(value)
          val extra = value.drop(sig.size)
          parseInputMap(next, accum :+ FinalizedScriptSig(sig, extra))
        case FinalizedScriptWitnessKey =>
          require(
            key.size == 1,
            s"The key must only contain the 1 byte type, got: ${key.size}")

          val scriptWit = RawScriptWitnessParser.read(value)
          parseInputMap(next, accum :+ FinalizedScriptWitness(scriptWit))
        case ProofOfReservesCommitmentKey =>
          parseInputMap(next, accum :+ ProofOfReservesCommitment(value))
        case _ =>
          parseInputMap(next, accum :+ InputPSBTRecord.Unknown(key, value))
      }
    }
  }

  def parseOutputMaps(
      bytes: ByteVector,
      numOutputs: Int): PSBTParseResult[OutputPSBTMap] = {
    parseOutputMaps(bytes, numOutputs, Nil)
  }

  @tailrec
  private def parseOutputMaps(
      remainingBytes: ByteVector,
      remainingOutputs: Int,
      accum: Seq[OutputPSBTMap]): PSBTParseResult[OutputPSBTMap] = {
    if (remainingOutputs <= 0 || remainingBytes.isEmpty) {
      PSBTParseResult[OutputPSBTMap](remainingBytes, accum.toVector)
    } else {

      val (next, map) = PSBTHelper.parseOutputMap(remainingBytes, Nil)

      parseOutputMaps(next, remainingOutputs - 1, accum :+ OutputPSBTMap(map))
    }
  }

  @tailrec
  private def parseOutputMap(
      remainingBytes: ByteVector,
      accum: Seq[OutputPSBTRecord]): (ByteVector, Vector[OutputPSBTRecord]) = {
    if (remainingBytes.isEmpty || remainingBytes.head == terminator) {
      (remainingBytes.tail, accum.toVector)
    } else {
      val (totalSize, key, value) = parseKeyAndValue(remainingBytes)
      val next = remainingBytes.drop(totalSize) // Drop the key-value map

      PSBTOutputKey.fromByte(key.head) match {
        case PSBTOutputKey.RedeemScriptKey =>
          require(
            key.size == 1,
            s"The key must only contain the 1 byte type, got: ${key.size}")

          val redeemScript = ScriptPubKey.fromAsmBytes(value)
          parseOutputMap(next,
                         accum :+ OutputPSBTRecord.RedeemScript(redeemScript))
        case PSBTOutputKey.WitnessScriptKey =>
          require(
            key.size == 1,
            s"The key must only contain the 1 byte type, got: ${key.size}")

          val witnessScript = ScriptPubKey.fromAsmBytes(value)
          parseOutputMap(next,
                         accum :+ OutputPSBTRecord.WitnessScript(witnessScript))
        case PSBTOutputKey.BIP32DerivationPathKey =>
          val pubKey = ECPublicKey(key.tail)
          val fingerprint = value.take(4)
          val path = BIP32Path.fromBytesLE(value.drop(4))
          parseOutputMap(
            next,
            accum :+ OutputPSBTRecord.BIP32DerivationPath(pubKey,
                                                          fingerprint,
                                                          path))
        case _ =>
          parseOutputMap(next, accum :+ OutputPSBTRecord.Unknown(key, value))
      }
    }
  }

}
