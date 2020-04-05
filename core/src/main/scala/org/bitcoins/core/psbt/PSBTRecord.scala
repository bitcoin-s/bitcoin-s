package org.bitcoins.core.psbt

import org.bitcoins.core.crypto.{
  DummyECDigitalSignature,
  ECDigitalSignature,
  ECPublicKey,
  ExtPublicKey
}
import org.bitcoins.core.hd.BIP32Path
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction.{
  BaseTransaction,
  Transaction,
  TransactionOutput
}
import org.bitcoins.core.protocol.{CompactSizeUInt, NetworkElement}
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.serializers.script.RawScriptWitnessParser
import org.bitcoins.core.util.{BitcoinSUtil, Factory}
import scodec.bits.ByteVector

sealed trait PSBTRecord extends NetworkElement {

  type KeyId <: PSBTKeyId

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
      val key = bytes.drop(keyCmpctUInt.byteSize).take(keyCmpctUInt.toLong)
      val valueBytes = bytes.drop(keyCmpctUInt.byteSize + keyCmpctUInt.toLong)
      val valueCmpctUInt = CompactSizeUInt.parse(valueBytes)
      val value = valueBytes
        .drop(valueCmpctUInt.byteSize)
        .take(valueCmpctUInt.toLong)

      (key, value)
    }
  }
}

sealed trait GlobalPSBTRecord extends PSBTRecord {
  override type KeyId <: PSBTGlobalKeyId
}

object GlobalPSBTRecord extends Factory[GlobalPSBTRecord] {
  import org.bitcoins.core.psbt.PSBTGlobalKeyId._
  case class UnsignedTransaction(transaction: BaseTransaction)
      extends GlobalPSBTRecord {
    require(
      transaction.inputs.forall(_.scriptSignature == EmptyScriptSignature),
      s"All ScriptSignatures must be empty, got $transaction")

    override type KeyId = UnsignedTransactionKeyId.type
    override val key: ByteVector = ByteVector(UnsignedTransactionKeyId.byte)
    override val value: ByteVector = transaction.bytes
  }

  case class XPubKey(
      xpub: ExtPublicKey,
      masterFingerprint: ByteVector,
      derivationPath: BIP32Path)
      extends GlobalPSBTRecord {
    require(
      derivationPath.length == xpub.depth.toInt,
      s"Derivation path length does not match xpubkey depth, difference: ${derivationPath.length - xpub.depth.toInt}"
    )
    require(
      masterFingerprint.length == 4,
      s"Master key fingerprints are 4 bytes long, got: $masterFingerprint")

    override type KeyId = XPubKeyKeyId.type
    override val key: ByteVector = ByteVector(XPubKeyKeyId.byte) ++ xpub.bytes
    override val value: ByteVector = derivationPath
      .foldLeft(masterFingerprint)(_ ++ _.toUInt32.bytesLE)
  }

  case class Version(version: UInt32) extends GlobalPSBTRecord {
    override type KeyId = VersionKeyId.type
    override val key: ByteVector = ByteVector(VersionKeyId.byte)
    override val value: ByteVector = version.bytesLE
  }

  case class Unknown(key: ByteVector, value: ByteVector)
      extends GlobalPSBTRecord {
    override type KeyId = UnknownKeyId.type
    private val keyId = PSBTGlobalKeyId.fromBytes(key)
    require(keyId == UnknownKeyId,
            s"Cannot make an Unknown record with a $keyId")
  }

  override def fromBytes(bytes: ByteVector): GlobalPSBTRecord = {
    import org.bitcoins.core.psbt.PSBTGlobalKeyId._

    val (key, value) = PSBTRecord.fromBytes(bytes)
    PSBTGlobalKeyId.fromByte(key.head) match {
      case UnsignedTransactionKeyId =>
        require(key.size == 1,
                s"The key must only contain the 1 byte type, got: ${key.size}")

        UnsignedTransaction(BaseTransaction.fromBytes(value))
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
      case UnknownKeyId =>
        GlobalPSBTRecord.Unknown(key, value)
    }
  }
}

sealed trait InputPSBTRecord extends PSBTRecord {
  override type KeyId <: PSBTInputKeyId
}

object InputPSBTRecord extends Factory[InputPSBTRecord] {
  import org.bitcoins.core.psbt.PSBTInputKeyId._
  case class NonWitnessOrUnknownUTXO(transactionSpent: Transaction)
      extends InputPSBTRecord {
    override type KeyId = NonWitnessUTXOKeyId.type
    override val key: ByteVector = ByteVector(NonWitnessUTXOKeyId.byte)
    override val value: ByteVector = transactionSpent.bytes
  }

  case class WitnessUTXO(witnessUTXO: TransactionOutput)
      extends InputPSBTRecord {
    override type KeyId = WitnessUTXOKeyId.type
    override val key: ByteVector = ByteVector(WitnessUTXOKeyId.byte)
    override val value: ByteVector = witnessUTXO.bytes
  }

  case class PartialSignature(
      pubKey: ECPublicKey,
      signature: ECDigitalSignature)
      extends InputPSBTRecord {
    require(pubKey.byteSize == 33,
            s"pubKey must be 33 bytes, got: ${pubKey.byteSize}")

    override type KeyId = PartialSignatureKeyId.type
    override val key: ByteVector = ByteVector(PartialSignatureKeyId.byte) ++ pubKey.bytes
    override val value: ByteVector = signature.bytes
  }

  object PartialSignature extends Factory[PartialSignature] {

    def dummyPartialSig(
        pubKey: ECPublicKey = ECPublicKey.freshPublicKey): PartialSignature = {
      PartialSignature(pubKey, DummyECDigitalSignature)
    }

    override def fromBytes(bytes: ByteVector): PartialSignature =
      InputPSBTRecord(bytes) match {
        case partialSignature: PartialSignature =>
          partialSignature
        case other: InputPSBTRecord =>
          throw new IllegalArgumentException(
            s"Invalid PartialSignature encoding, got: $other")
      }

    def vecFromBytes(bytes: ByteVector): Vector[PartialSignature] = {
      @scala.annotation.tailrec
      def loop(
          remainingBytes: ByteVector,
          accum: Vector[PartialSignature]): Vector[PartialSignature] = {
        if (remainingBytes.isEmpty) {
          accum
        } else {
          val record = fromBytes(remainingBytes)
          val next = remainingBytes.drop(record.bytes.size)

          loop(next, accum :+ record)
        }
      }

      loop(bytes, Vector.empty)
    }

    def vecFromHex(hex: String): Vector[PartialSignature] = {
      vecFromBytes(BitcoinSUtil.decodeHex(hex))
    }
  }

  case class SigHashType(hashType: HashType) extends InputPSBTRecord {
    override type KeyId = SigHashTypeKeyId.type
    override val key: ByteVector = ByteVector(SigHashTypeKeyId.byte)
    override val value: ByteVector = hashType.num.bytesLE
  }

  case class RedeemScript(redeemScript: ScriptPubKey) extends InputPSBTRecord {
    override type KeyId = RedeemScriptKeyId.type
    override val key: ByteVector = ByteVector(RedeemScriptKeyId.byte)
    override val value: ByteVector = redeemScript.asmBytes
  }

  case class WitnessScript(witnessScript: RawScriptPubKey)
      extends InputPSBTRecord {
    override type KeyId = WitnessScriptKeyId.type
    override val key: ByteVector = ByteVector(WitnessScriptKeyId.byte)
    override val value: ByteVector = witnessScript.asmBytes
  }

  case class BIP32DerivationPath(
      pubKey: ECPublicKey,
      masterFingerprint: ByteVector,
      path: BIP32Path)
      extends InputPSBTRecord {
    require(pubKey.byteSize == 33,
            s"pubKey must be 33 bytes, got: ${pubKey.byteSize}")

    override type KeyId = BIP32DerivationPathKeyId.type
    override val key: ByteVector = ByteVector(BIP32DerivationPathKeyId.byte) ++ pubKey.bytes
    override val value: ByteVector =
      path.foldLeft(masterFingerprint)(_ ++ _.toUInt32.bytesLE)
  }

  case class FinalizedScriptSig(scriptSig: ScriptSignature)
      extends InputPSBTRecord {
    override type KeyId = FinalizedScriptSigKeyId.type
    override val key: ByteVector = ByteVector(FinalizedScriptSigKeyId.byte)
    override val value: ByteVector = scriptSig.asmBytes
  }

  case class FinalizedScriptWitness(scriptWitness: ScriptWitness)
      extends InputPSBTRecord {
    override type KeyId = FinalizedScriptWitnessKeyId.type
    override val key: ByteVector = ByteVector(FinalizedScriptWitnessKeyId.byte)
    override val value: ByteVector = scriptWitness.bytes
  }

  case class ProofOfReservesCommitment(porCommitment: ByteVector)
      extends InputPSBTRecord {
    override type KeyId = ProofOfReservesCommitmentKeyId.type
    override val key: ByteVector = ByteVector(
      ProofOfReservesCommitmentKeyId.byte)
    override val value: ByteVector = porCommitment
  }

  case class Unknown(key: ByteVector, value: ByteVector)
      extends InputPSBTRecord {
    override type KeyId = UnknownKeyId.type
    private val keyId = PSBTInputKeyId.fromBytes(key)
    require(keyId == UnknownKeyId,
            s"Cannot make an Unknown record with a $keyId")
  }

  override def fromBytes(bytes: ByteVector): InputPSBTRecord = {
    import org.bitcoins.core.psbt.PSBTInputKeyId._

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
      case SigHashTypeKeyId =>
        require(key.size == 1,
                s"The key must only contain the 1 byte type, got: ${key.size}")

        SigHashType(HashType.fromBytesLE(value))
      case PSBTInputKeyId.RedeemScriptKeyId =>
        require(key.size == 1,
                s"The key must only contain the 1 byte type, got: ${key.size}")

        InputPSBTRecord.RedeemScript(ScriptPubKey.fromAsmBytes(value))
      case PSBTInputKeyId.WitnessScriptKeyId =>
        require(key.size == 1,
                s"The key must only contain the 1 byte type, got: ${key.size}")

        InputPSBTRecord.WitnessScript(RawScriptPubKey.fromAsmBytes(value))
      case PSBTInputKeyId.BIP32DerivationPathKeyId =>
        val pubKey = ECPublicKey(key.tail)
        val fingerprint = value.take(4)
        val path = BIP32Path.fromBytesLE(value.drop(4))
        InputPSBTRecord.BIP32DerivationPath(pubKey, fingerprint, path)
      case FinalizedScriptSigKeyId =>
        require(key.size == 1,
                s"The key must only contain the 1 byte type, got: ${key.size}")

        val sig = ScriptSignature.fromAsmBytes(value)
        FinalizedScriptSig(sig)
      case FinalizedScriptWitnessKeyId =>
        require(key.size == 1,
                s"The key must only contain the 1 byte type, got: ${key.size}")

        FinalizedScriptWitness(RawScriptWitnessParser.read(value))
      case ProofOfReservesCommitmentKeyId =>
        ProofOfReservesCommitment(value)
      case UnknownKeyId =>
        InputPSBTRecord.Unknown(key, value)
    }
  }
}

sealed trait OutputPSBTRecord extends PSBTRecord {
  override type KeyId <: PSBTOutputKeyId
}

object OutputPSBTRecord extends Factory[OutputPSBTRecord] {
  import org.bitcoins.core.psbt.PSBTOutputKeyId._

  case class RedeemScript(redeemScript: ScriptPubKey) extends OutputPSBTRecord {
    override type KeyId = RedeemScriptKeyId.type
    override val key: ByteVector = ByteVector(RedeemScriptKeyId.byte)
    override val value: ByteVector = redeemScript.asmBytes
  }

  case class WitnessScript(witnessScript: ScriptPubKey)
      extends OutputPSBTRecord {
    override type KeyId = WitnessScriptKeyId.type
    override val key: ByteVector = ByteVector(WitnessScriptKeyId.byte)
    override val value: ByteVector = witnessScript.asmBytes
  }

  case class BIP32DerivationPath(
      pubKey: ECPublicKey,
      masterFingerprint: ByteVector,
      path: BIP32Path)
      extends OutputPSBTRecord {
    require(pubKey.byteSize == 33,
            s"pubKey must be 33 bytes, got: ${pubKey.byteSize}")

    override type KeyId = BIP32DerivationPathKeyId.type
    override val key: ByteVector = ByteVector(BIP32DerivationPathKeyId.byte) ++ pubKey.bytes
    override val value: ByteVector =
      path.foldLeft(masterFingerprint)(_ ++ _.toUInt32.bytesLE)
  }

  case class Unknown(key: ByteVector, value: ByteVector)
      extends OutputPSBTRecord {
    override type KeyId = UnknownKeyId.type
    private val keyId = PSBTOutputKeyId.fromBytes(key)
    require(keyId == UnknownKeyId,
            s"Cannot make an Unknown record with a $keyId")
  }

  override def fromBytes(bytes: ByteVector): OutputPSBTRecord = {
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
      case UnknownKeyId =>
        OutputPSBTRecord.Unknown(key, value)
    }
  }
}
