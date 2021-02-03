package org.bitcoins.core.psbt

import org.bitcoins.crypto.Factory
import scodec.bits.ByteVector

/** A PSBTKeyId refers to the first byte of a key that signifies which kind of key-value map
  * is in a given PSBTRecord
  */
sealed trait PSBTKeyId {
  def byte: Byte
  type RecordType <: PSBTRecord
}

sealed trait PSBTKeyIdFactory[KeyIdType <: PSBTKeyId]
    extends Factory[KeyIdType] {
  def fromByte(byte: Byte): KeyIdType
  def unknownKey: KeyIdType

  override def fromBytes(key: ByteVector): KeyIdType = {
    if (key.isEmpty) {
      unknownKey
    } else {
      fromByte(key.head)
    }
  }
}

sealed trait PSBTGlobalKeyId extends PSBTKeyId {
  type RecordType <: GlobalPSBTRecord
}

object PSBTGlobalKeyId extends PSBTKeyIdFactory[PSBTGlobalKeyId] {

  override def fromByte(byte: Byte): PSBTGlobalKeyId =
    byte match {
      case UnsignedTransactionKeyId.byte => UnsignedTransactionKeyId
      case XPubKeyKeyId.byte             => XPubKeyKeyId
      case VersionKeyId.byte             => VersionKeyId
      case _: Byte                       => UnknownKeyId
    }

  override def unknownKey: PSBTGlobalKeyId = UnknownKeyId

  final case object UnsignedTransactionKeyId extends PSBTGlobalKeyId {
    override val byte: Byte = 0x00.byteValue
    type RecordType = GlobalPSBTRecord.UnsignedTransaction
  }

  final case object XPubKeyKeyId extends PSBTGlobalKeyId {
    override val byte: Byte = 0x01.byteValue
    type RecordType = GlobalPSBTRecord.XPubKey
  }

  final case object VersionKeyId extends PSBTGlobalKeyId {
    override val byte: Byte = 0xfb.byteValue
    type RecordType = GlobalPSBTRecord.Version
  }

  final case object UnknownKeyId extends PSBTGlobalKeyId {
    override val byte: Byte = Byte.MaxValue
    type RecordType = GlobalPSBTRecord.Unknown
  }
}

sealed trait PSBTInputKeyId extends PSBTKeyId {
  type RecordType <: InputPSBTRecord
}

object PSBTInputKeyId extends PSBTKeyIdFactory[PSBTInputKeyId] {

  override def unknownKey: PSBTInputKeyId = UnknownKeyId

  override def fromByte(byte: Byte): PSBTInputKeyId =
    byte match {
      case NonWitnessUTXOKeyId.byte            => NonWitnessUTXOKeyId
      case WitnessUTXOKeyId.byte               => WitnessUTXOKeyId
      case PartialSignatureKeyId.byte          => PartialSignatureKeyId
      case SigHashTypeKeyId.byte               => SigHashTypeKeyId
      case RedeemScriptKeyId.byte              => RedeemScriptKeyId
      case WitnessScriptKeyId.byte             => WitnessScriptKeyId
      case BIP32DerivationPathKeyId.byte       => BIP32DerivationPathKeyId
      case FinalizedScriptSigKeyId.byte        => FinalizedScriptSigKeyId
      case FinalizedScriptWitnessKeyId.byte    => FinalizedScriptWitnessKeyId
      case ProofOfReservesCommitmentKeyId.byte => ProofOfReservesCommitmentKeyId
      case RIPEMD160PreImageKeyId.byte         => RIPEMD160PreImageKeyId
      case SHA256PreImageKeyId.byte            => SHA256PreImageKeyId
      case HASH160PreImageKeyId.byte           => HASH160PreImageKeyId
      case HASH256PreImageKeyId.byte           => HASH256PreImageKeyId
      case _: Byte                             => UnknownKeyId

    }

  final case object NonWitnessUTXOKeyId extends PSBTInputKeyId {
    override val byte: Byte = 0x00.byteValue
    type RecordType = InputPSBTRecord.NonWitnessOrUnknownUTXO
  }

  final case object WitnessUTXOKeyId extends PSBTInputKeyId {
    override val byte: Byte = 0x01.byteValue
    type RecordType = InputPSBTRecord.WitnessUTXO
  }

  final case object PartialSignatureKeyId extends PSBTInputKeyId {
    override val byte: Byte = 0x02.byteValue
    type RecordType = InputPSBTRecord.PartialSignature
  }

  final case object SigHashTypeKeyId extends PSBTInputKeyId {
    override val byte: Byte = 0x03.byteValue
    type RecordType = InputPSBTRecord.SigHashType
  }

  final case object RedeemScriptKeyId extends PSBTInputKeyId {
    override val byte: Byte = 0x04.byteValue
    type RecordType = InputPSBTRecord.RedeemScript
  }

  final case object WitnessScriptKeyId extends PSBTInputKeyId {
    override val byte: Byte = 0x05.byteValue
    type RecordType = InputPSBTRecord.WitnessScript
  }

  final case object BIP32DerivationPathKeyId extends PSBTInputKeyId {
    override val byte: Byte = 0x06.byteValue
    type RecordType = InputPSBTRecord.BIP32DerivationPath
  }

  final case object FinalizedScriptSigKeyId extends PSBTInputKeyId {
    override val byte: Byte = 0x07.byteValue
    type RecordType = InputPSBTRecord.FinalizedScriptSig
  }

  final case object FinalizedScriptWitnessKeyId extends PSBTInputKeyId {
    override val byte: Byte = 0x08.byteValue
    type RecordType = InputPSBTRecord.FinalizedScriptWitness
  }

  final case object ProofOfReservesCommitmentKeyId extends PSBTInputKeyId {
    override val byte: Byte = 0x09.byteValue
    type RecordType = InputPSBTRecord.ProofOfReservesCommitment
  }

  final case object RIPEMD160PreImageKeyId extends PSBTInputKeyId {
    override val byte: Byte = 0x0a.byteValue
    type RecordType = InputPSBTRecord.RIPEMD160PreImage
  }

  final case object SHA256PreImageKeyId extends PSBTInputKeyId {
    override val byte: Byte = 0x0b.byteValue
    type RecordType = InputPSBTRecord.SHA256PreImage
  }

  final case object HASH160PreImageKeyId extends PSBTInputKeyId {
    override val byte: Byte = 0x0c.byteValue
    type RecordType = InputPSBTRecord.HASH160PreImage
  }

  final case object HASH256PreImageKeyId extends PSBTInputKeyId {
    override val byte: Byte = 0x0d.byteValue
    type RecordType = InputPSBTRecord.HASH256PreImage
  }

  final case object UnknownKeyId extends PSBTInputKeyId {
    override val byte: Byte = Byte.MaxValue
    type RecordType = InputPSBTRecord.Unknown
  }
}

sealed trait PSBTOutputKeyId extends PSBTKeyId {
  type RecordType <: OutputPSBTRecord
}

object PSBTOutputKeyId extends PSBTKeyIdFactory[PSBTOutputKeyId] {

  override def unknownKey: PSBTOutputKeyId = UnknownKeyId

  override def fromByte(byte: Byte): PSBTOutputKeyId =
    byte match {
      case RedeemScriptKeyId.byte        => RedeemScriptKeyId
      case WitnessScriptKeyId.byte       => WitnessScriptKeyId
      case BIP32DerivationPathKeyId.byte => BIP32DerivationPathKeyId
      case _: Byte                       => UnknownKeyId
    }

  final case object RedeemScriptKeyId extends PSBTOutputKeyId {
    override val byte: Byte = 0x00.byteValue
    type RecordType = OutputPSBTRecord.RedeemScript
  }

  final case object WitnessScriptKeyId extends PSBTOutputKeyId {
    override val byte: Byte = 0x01.byteValue
    type RecordType = OutputPSBTRecord.WitnessScript
  }

  final case object BIP32DerivationPathKeyId extends PSBTOutputKeyId {
    override val byte: Byte = 0x02.byteValue
    type RecordType = OutputPSBTRecord.BIP32DerivationPath
  }

  final case object UnknownKeyId extends PSBTOutputKeyId {
    override val byte: Byte = Byte.MaxValue
    type RecordType = OutputPSBTRecord.Unknown
  }
}
