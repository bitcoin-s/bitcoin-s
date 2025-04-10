package org.bitcoins.core.psbt

import org.bitcoins.core.crypto.ExtPublicKey
import org.bitcoins.core.hd.BIP32Path
import org.bitcoins.core.number.{Int32, UInt32, UInt64}
import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.protocol.script.*
import org.bitcoins.core.protocol.transaction.{
  BaseTransaction,
  NonWitnessTransaction,
  Transaction,
  TransactionOutput
}
import org.bitcoins.core.serializers.script.RawScriptWitnessParser
import org.bitcoins.core.util.BytesUtil
import org.bitcoins.crypto.*
import scodec.bits.ByteVector

import scala.annotation.tailrec

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
  import org.bitcoins.core.psbt.PSBTGlobalKeyId.*

  case class UnsignedTransaction(transaction: NonWitnessTransaction)
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
    import org.bitcoins.core.psbt.PSBTGlobalKeyId.*

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
  import org.bitcoins.core.psbt.PSBTInputKeyId.*

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

  case class PartialSignature[Sig <: DigitalSignature](
      pubKey: ECPublicKeyBytes,
      signature: Sig)
      extends InputPSBTRecord {
    require(pubKey.byteSize == 33,
            s"pubKey must be 33 bytes, got: ${pubKey.byteSize}")

    override type KeyId = PartialSignatureKeyId[DigitalSignature]

    override val key: ByteVector =
      ByteVector(PartialSignatureKeyId.byte) ++ pubKey.bytes
    override val value: ByteVector = signature.bytes
  }

  object PartialSignature extends Factory[PartialSignature[DigitalSignature]] {

    def apply(
        pubKey: ECPublicKey,
        signature: ECDigitalSignature): PartialSignature[ECDigitalSignature] = {
      PartialSignature(pubKey.toPublicKeyBytes(), signature)
    }

    def apply(pubKey: ECPublicKey, signature: SchnorrDigitalSignature)
        : PartialSignature[SchnorrDigitalSignature] = {
      PartialSignature(pubKey.toPublicKeyBytes(), signature)
    }

    def dummyPartialSig(pubKey: ECPublicKey = ECPublicKey.freshPublicKey)
        : PartialSignature[ECDigitalSignature] = {
      PartialSignature(pubKey, ECDigitalSignature.dummy)
    }

    override def fromBytes(
        bytes: ByteVector): PartialSignature[DigitalSignature] =
      InputPSBTRecord(bytes) match {
        case partialSignature: PartialSignature[DigitalSignature] @unchecked =>
          partialSignature
        case other: InputPSBTRecord =>
          throw new IllegalArgumentException(
            s"Invalid PartialSignature encoding, got: $other")
      }

    def vecFromBytes(bytes: ByteVector): Vector[PartialSignature[?]] = {
      @scala.annotation.tailrec
      def loop(
          remainingBytes: ByteVector,
          accum: Vector[PartialSignature[?]]): Vector[PartialSignature[?]] = {
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

    def vecFromHex(hex: String): Vector[PartialSignature[?]] = {
      vecFromBytes(BytesUtil.decodeHex(hex))
    }
  }

  case class SigHashType(hashType: HashType) extends InputPSBTRecord {
    override type KeyId = SigHashTypeKeyId.type
    override val key: ByteVector = ByteVector(SigHashTypeKeyId.byte)
    override val value: ByteVector = Int32(hashType.num).bytesLE
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

    override val key: ByteVector =
      ByteVector(BIP32DerivationPathKeyId.byte) ++ pubKey.bytes

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

  case class RIPEMD160PreImage(preImage: ByteVector) extends InputPSBTRecord {
    override type KeyId = RIPEMD160PreImageKeyId.type

    val hash: RipeMd160Digest = CryptoUtil.ripeMd160(preImage)

    lazy val hashBE: RipeMd160DigestBE = hash.flip

    override val key: ByteVector =
      ByteVector(RIPEMD160PreImageKeyId.byte) ++ hash.bytes
    override val value: ByteVector = preImage
  }

  case class SHA256PreImage(preImage: ByteVector) extends InputPSBTRecord {
    override type KeyId = SHA256PreImageKeyId.type

    val hash: Sha256Digest = CryptoUtil.sha256(preImage)

    lazy val hashBE: Sha256DigestBE = hash.flip

    override val key: ByteVector =
      ByteVector(SHA256PreImageKeyId.byte) ++ hash.bytes
    override val value: ByteVector = preImage
  }

  case class HASH160PreImage(preImage: ByteVector) extends InputPSBTRecord {
    override type KeyId = HASH160PreImageKeyId.type

    val hash: Sha256Hash160Digest = CryptoUtil.sha256Hash160(preImage)

    lazy val hashBE: Sha256Hash160DigestBE = hash.flip

    override val key: ByteVector =
      ByteVector(HASH160PreImageKeyId.byte) ++ hash.bytes
    override val value: ByteVector = preImage
  }

  case class HASH256PreImage(preImage: ByteVector) extends InputPSBTRecord {
    override type KeyId = HASH256PreImageKeyId.type

    val hash: DoubleSha256Digest = CryptoUtil.doubleSHA256(preImage)

    lazy val hashBE: DoubleSha256DigestBE = hash.flip

    override val key: ByteVector =
      ByteVector(HASH256PreImageKeyId.byte) ++ hash.bytes
    override val value: ByteVector = preImage
  }

  case class TRKeySpendSignature(signature: SchnorrDigitalSignature)
      extends InputPSBTRecord {
    override type KeyId = TRKeySpendSignatureKeyId.type

    override val key: ByteVector = ByteVector(TRKeySpendSignatureKeyId.byte)
    override val value: ByteVector = signature.bytes
  }

  case class TRScriptSpendSignature(
      xOnlyPubKey: XOnlyPubKey,
      leafHash: Sha256Digest,
      signature: SchnorrDigitalSignature)
      extends InputPSBTRecord {
    override type KeyId = TRScriptSpendSignatureKeyId.type

    override val key: ByteVector = ByteVector(
      TRScriptSpendSignatureKeyId.byte) ++ xOnlyPubKey.bytes ++ leafHash.bytes
    override val value: ByteVector = signature.bytes
  }

  case class TRLeafScript(
      controlBlock: ControlBlock,
      script: RawScriptPubKey,
      leafVersion: LeafVersion)
      extends InputPSBTRecord {
    override type KeyId = TRLeafScriptKeyId.type

    override val key: ByteVector = {
      ByteVector(TRLeafScriptKeyId.byte) ++ controlBlock.bytes
    }

    override val value: ByteVector = {
      script.asmBytes ++ ByteVector.fromByte(leafVersion.toByte)
    }
  }

  case class TRBIP32DerivationPath(
      xOnlyPubKey: XOnlyPubKey,
      hashes: Vector[Sha256Digest],
      masterFingerprint: ByteVector,
      path: BIP32Path)
      extends InputPSBTRecord {

    override type KeyId = TRBIP32DerivationPathKeyId.type

    override val key: ByteVector =
      ByteVector(TRBIP32DerivationPathKeyId.byte) ++ xOnlyPubKey.bytes

    override val value: ByteVector = {
      val hashesBytes = if (hashes.isEmpty) {
        CompactSizeUInt.zero.bytes
      } else {
        CompactSizeUInt(UInt64(hashes.length)).bytes ++
          hashes.map(_.bytes).reduce(_ ++ _)
      }
      hashesBytes ++ path.foldLeft(masterFingerprint)(_ ++ _.toUInt32.bytesLE)
    }
  }

  case class TRInternalKey(xOnlyPubKey: XOnlyPubKey) extends InputPSBTRecord {
    override type KeyId = TRInternalKeyKeyId.type

    override val key: ByteVector = ByteVector(TRInternalKeyKeyId.byte)
    override val value: ByteVector = xOnlyPubKey.bytes
  }

  case class TRMerkelRoot(hash: Sha256Digest) extends InputPSBTRecord {
    override type KeyId = TRMerkelRootKeyId.type

    override val key: ByteVector = ByteVector(TRMerkelRootKeyId.byte)
    override val value: ByteVector = hash.bytes
  }

  case class Unknown(key: ByteVector, value: ByteVector)
      extends InputPSBTRecord {
    override type KeyId = UnknownKeyId.type
    private val keyId = PSBTInputKeyId.fromBytes(key)
    require(keyId == UnknownKeyId,
            s"Cannot make an Unknown record with a $keyId")
  }

  override def fromBytes(bytes: ByteVector): InputPSBTRecord = {
    import org.bitcoins.core.psbt.PSBTInputKeyId.*

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
      case PartialSignatureKeyId() =>
        val pubKey = ECPublicKey(key.tail)
        if (value.length == 64 || value.length == 65) {
          val sig = SchnorrDigitalSignature(value)
          PartialSignature(pubKey.toPublicKeyBytes(), sig)
        } else {
          val sig = ECDigitalSignature(value)
          PartialSignature(pubKey.toPublicKeyBytes(), sig)
        }
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
      case RIPEMD160PreImageKeyId =>
        require(
          key.size == 21,
          s"The key must contain the 1 byte type followed by the 20 byte hash, got: $key")

        val hash = key.tail
        val record = RIPEMD160PreImage(value)
        require(record.hash.bytes == hash,
                "Received invalid RIPEMD160PreImage, hash does not match")
        record
      case SHA256PreImageKeyId =>
        require(
          key.size == 33,
          s"The key must contain the 1 byte type followed by the 32 byte hash, got: $key")

        val hash = key.tail
        val record = SHA256PreImage(value)
        require(record.hash.bytes == hash,
                "Received invalid SHA256PreImage, hash does not match")
        record
      case HASH160PreImageKeyId =>
        require(
          key.size == 21,
          s"The key must contain the 1 byte type followed by the 20 byte hash, got: $key")

        val hash = key.tail
        val record = HASH160PreImage(value)
        require(record.hash.bytes == hash,
                "Received invalid HASH160PreImage, hash does not match")
        record
      case HASH256PreImageKeyId =>
        require(
          key.size == 21,
          s"The key must contain the 1 byte type followed by the 32 byte hash, got: $key")

        val hash = key.tail
        val record = HASH256PreImage(value)
        require(record.hash.bytes == hash,
                "Received invalid HASH256PreImage, hash does not match")
        record
      case TRKeySpendSignatureKeyId =>
        require(key.size == 1,
                s"The key must only contain the 1 byte type, got: ${key.size}")

        val sig = SchnorrDigitalSignature.fromBytes(value)
        TRKeySpendSignature(sig)
      case TRScriptSpendSignatureKeyId =>
        require(
          key.size == 65,
          s"The key must only contain the 65 bytes type, got: ${key.size}")

        val (xOnlyPubKey, leafHash) = key.tail.splitAt(32)
        val sig = SchnorrDigitalSignature.fromBytes(value)

        TRScriptSpendSignature(XOnlyPubKey(xOnlyPubKey),
                               Sha256Digest(leafHash),
                               sig)

      case TRLeafScriptKeyId =>
        val controlBlock = ControlBlock(key.tail)

        val script = RawScriptPubKey.fromAsmBytes(value.init)

        TRLeafScript(controlBlock, script, LeafVersion.fromByte(value.last))
      case TRBIP32DerivationPathKeyId =>
        val pubKey = XOnlyPubKey(key.tail)
        val numHashes = CompactSizeUInt.fromBytes(value)
        val hashes = value
          .drop(numHashes.byteSize)
          .take(numHashes.num.toInt * 32)
          .grouped(32)
          .map(Sha256Digest.fromBytes)
          .toVector
        val remaining = value.drop(numHashes.byteSize + hashes.size * 32)
        val fingerprint = remaining.take(4)
        val path = BIP32Path.fromBytesLE(remaining.drop(4))

        TRBIP32DerivationPath(xOnlyPubKey = pubKey,
                              hashes = hashes,
                              masterFingerprint = fingerprint,
                              path = path)
      case TRInternalKeyKeyId =>
        require(key.size == 1,
                s"The key must only contain the 1 byte type, got: ${key.size}")

        require(
          value.size == 32,
          s"The value must contain the 32 byte x-only public key, got: ${value.size}")
        TRInternalKey(XOnlyPubKey.fromBytes(value))
      case TRMerkelRootKeyId =>
        require(key.size == 1,
                s"The key must only contain the 1 byte type, got: ${key.size}")

        require(
          value.size == 32,
          s"The value must contain the 32 byte x-only public key, got: ${value.size}")
        TRMerkelRoot(Sha256Digest(value))
      case UnknownKeyId =>
        InputPSBTRecord.Unknown(key, value)
    }
  }
}

sealed trait OutputPSBTRecord extends PSBTRecord {
  override type KeyId <: PSBTOutputKeyId
}

object OutputPSBTRecord extends Factory[OutputPSBTRecord] {
  import org.bitcoins.core.psbt.PSBTOutputKeyId.*

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

    override val key: ByteVector =
      ByteVector(BIP32DerivationPathKeyId.byte) ++ pubKey.bytes

    override val value: ByteVector =
      path.foldLeft(masterFingerprint)(_ ++ _.toUInt32.bytesLE)
  }

  case class TaprootTree(
      leafs: Vector[
        (Byte, Byte, ByteVector)
      ] // todo change to TapScriptPubKey when we have a TapScriptPubKey type
  ) extends OutputPSBTRecord {
    require(leafs.nonEmpty)
    override type KeyId = TaprootTreeKeyId.type

    override val key: ByteVector =
      ByteVector(TaprootTreeKeyId.byte)

    override val value: ByteVector = {
      leafs.foldLeft(ByteVector.empty) { (acc, leaf) =>
        val spk = leaf._3
        acc ++ ByteVector.fromByte(leaf._1) ++ ByteVector.fromByte(
          leaf._2) ++ CompactSizeUInt.calc(spk).bytes ++ spk
      }
    }
  }

  case class TRBIP32DerivationPath(
      xOnlyPubKey: XOnlyPubKey,
      hashes: Vector[Sha256Digest],
      masterFingerprint: ByteVector,
      path: BIP32Path)
      extends OutputPSBTRecord {

    override type KeyId = TRBIP32DerivationPathKeyId.type

    override val key: ByteVector =
      ByteVector(TRBIP32DerivationPathKeyId.byte) ++ xOnlyPubKey.bytes

    override val value: ByteVector = {
      val hashesBytes = if (hashes.isEmpty) {
        CompactSizeUInt.zero.bytes
      } else {
        CompactSizeUInt(UInt64(hashes.size)).bytes ++
          hashes.map(_.bytes).reduce(_ ++ _)
      }
      hashesBytes ++ path.foldLeft(masterFingerprint)(_ ++ _.toUInt32.bytesLE)
    }
  }

  case class TRInternalKey(xOnlyPubKey: XOnlyPubKey) extends OutputPSBTRecord {
    override type KeyId = TRInternalKeyKeyId.type

    override val key: ByteVector = ByteVector(TRInternalKeyKeyId.byte)

    override val value: ByteVector = xOnlyPubKey.bytes
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
      case PSBTOutputKeyId.TRInternalKeyKeyId =>
        require(key.size == 1,
                s"The key must only contain the 1 byte type, got: ${key.size}")

        val xOnlyPubKey = XOnlyPubKey.fromBytes(value)
        OutputPSBTRecord.TRInternalKey(xOnlyPubKey)
      case TaprootTreeKeyId =>
        @tailrec
        def loop(bytes: ByteVector, accum: Vector[(Byte, Byte, ByteVector)])
            : Vector[(Byte, Byte, ByteVector)] = {
          if (bytes.isEmpty) {
            accum
          } else {
            val depth = bytes.head
            val version = bytes.tail.head
            val spkLen = CompactSizeUInt.fromBytes(bytes.drop(2))
            val spk = bytes.drop(spkLen.byteSize + 2)

            val remaining = bytes.drop(2 + spkLen.byteSize + spk.length)
            loop(remaining, accum :+ (depth, version, spk))
          }
        }
        val leafs = loop(value, Vector.empty)
        OutputPSBTRecord.TaprootTree(leafs)
      case PSBTOutputKeyId.TRBIP32DerivationPathKeyId =>
        val pubKey = XOnlyPubKey(key.tail)
        val numHashes = CompactSizeUInt.fromBytes(value)
        val hashes = value
          .drop(numHashes.byteSize)
          .take(numHashes.num.toInt * 32)
          .grouped(32)
          .map(Sha256Digest.fromBytes)
          .toVector
        val remaining = value.drop(numHashes.byteSize + hashes.size * 32)
        val fingerprint = remaining.take(4)
        val path = BIP32Path.fromBytesLE(remaining.drop(4))

        OutputPSBTRecord.TRBIP32DerivationPath(xOnlyPubKey = pubKey,
                                               hashes = hashes,
                                               masterFingerprint = fingerprint,
                                               path = path)
      case UnknownKeyId =>
        OutputPSBTRecord.Unknown(key, value)
    }
  }
}
