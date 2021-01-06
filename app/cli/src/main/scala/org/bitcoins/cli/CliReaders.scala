package org.bitcoins.cli

import java.io.File
import java.nio.file.Path
import java.time.{Instant, ZoneId, ZonedDateTime}
import org.bitcoins.commons.jsonmodels.bitcoind.RpcOpts.LockUnspentOutputParameter
import org.bitcoins.core.protocol.dlc.DLCMessage._
import org.bitcoins.core.api.wallet.CoinSelectionAlgo
import org.bitcoins.core.config.{NetworkParameters, Networks}
import org.bitcoins.core.crypto.{ExtPrivateKey, MnemonicCode}
import org.bitcoins.core.currency._
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.BlockStamp.BlockTime
import org.bitcoins.core.protocol._
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionOutPoint}
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.core.psbt.PSBT
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.core.wallet.utxo.AddressLabelTag
import org.bitcoins.crypto._
import scodec.bits.ByteVector
import scopt._

/** scopt readers for parsing CLI params and options */
object CliReaders {

  implicit val pathReads: Read[Path] = new Read[Path] {
    val arity = 1

    val reads: String => Path = str => new File(str).toPath
  }

  implicit val npReads: Read[NetworkParameters] =
    new Read[NetworkParameters] {
      val arity: Int = 1

      val reads: String => NetworkParameters = str =>
        Networks.knownNetworks
          .find(_.toString.toLowerCase == str.toLowerCase)
          .getOrElse {
            val networks =
              Networks.knownNetworks
                .map(_.toString.toLowerCase)
                .mkString(", ")
            val msg =
              s"$str is not a valid network! Valid networks: $networks"
            sys.error(msg)
          }
    }

  implicit val byteVectorReads: Read[ByteVector] = new Read[ByteVector] {
    override def arity: Int = 1

    override def reads: String => ByteVector =
      str => ByteVector.fromValidHex(str)
  }

  implicit val doubleSha256Reads: Read[DoubleSha256DigestBE] =
    new Read[DoubleSha256DigestBE] {
      override def arity: Int = 1

      override def reads: String => DoubleSha256DigestBE =
        DoubleSha256DigestBE.fromHex
    }

  implicit val schnorrNonceReads: Read[SchnorrNonce] =
    new Read[SchnorrNonce] {
      override def arity: Int = 1

      override def reads: String => SchnorrNonce = SchnorrNonce.fromHex
    }

  implicit val eventDescriptorReads: Read[EventDescriptorTLV] =
    new Read[EventDescriptorTLV] {
      override def arity: Int = 1

      override def reads: String => EventDescriptorTLV =
        EventDescriptorTLV.fromHex
    }

  implicit val enumEventDescriptorReads: Read[EnumEventDescriptorV0TLV] =
    new Read[EnumEventDescriptorV0TLV] {
      override def arity: Int = 1

      override def reads: String => EnumEventDescriptorV0TLV =
        EnumEventDescriptorV0TLV.fromHex
    }

  implicit val rangeEventDescriptorReads: Read[RangeEventDescriptorV0TLV] =
    new Read[RangeEventDescriptorV0TLV] {
      override def arity: Int = 1

      override def reads: String => RangeEventDescriptorV0TLV =
        RangeEventDescriptorV0TLV.fromHex
    }

  implicit val digitDecompEventDescriptorReads: Read[
    DigitDecompositionEventDescriptorV0TLV] =
    new Read[DigitDecompositionEventDescriptorV0TLV] {
      override def arity: Int = 1

      override def reads: String => DigitDecompositionEventDescriptorV0TLV =
        DigitDecompositionEventDescriptorV0TLV.fromHex
    }

  implicit val oracleEventV0TLVReads: Read[OracleEventV0TLV] =
    new Read[OracleEventV0TLV] {
      override def arity: Int = 1

      override def reads: String => OracleEventV0TLV = OracleEventV0TLV.fromHex
    }

  implicit val instantReads: Read[Instant] =
    new Read[Instant] {
      override def arity: Int = 1

      override def reads: String => Instant =
        str => Instant.ofEpochSecond(str.toLong)
    }

  implicit val aesPasswordReads: Read[AesPassword] = new Read[AesPassword] {
    override def arity: Int = 1

    override def reads: String => AesPassword = AesPassword.fromString
  }

  implicit val bitcoinAddressReads: Read[BitcoinAddress] =
    new Read[BitcoinAddress] {
      val arity: Int = 1

      val reads: String => BitcoinAddress = BitcoinAddress.fromString
    }

  implicit val bitcoinsReads: Read[Bitcoins] =
    new Read[Bitcoins] {
      val arity: Int = 1
      val reads: String => Bitcoins = str => Bitcoins(BigDecimal(str))
    }

  implicit val satoshisReads: Read[Satoshis] =
    new Read[Satoshis] {
      val arity: Int = 1

      val reads: String => Satoshis = str => Satoshis(BigInt(str))
    }

  implicit val satoshisPerVirtualByteReads: Read[SatoshisPerVirtualByte] =
    new Read[SatoshisPerVirtualByte] {
      val arity: Int = 1

      val reads: String => SatoshisPerVirtualByte = str =>
        SatoshisPerVirtualByte(Satoshis(BigInt(str)))
    }

  implicit val uInt32Reads: Read[UInt32] = new Read[UInt32] {
    val arity: Int = 1

    val reads: String => UInt32 = str => UInt32(BigInt(str))
  }

  implicit val oracleInfoReads: Read[OracleInfo] = new Read[OracleInfo] {
    val arity: Int = 1
    val reads: String => OracleInfo = OracleInfo.fromHex
  }

  implicit val oracleAnnouncementReads: Read[OracleAnnouncementTLV] =
    new Read[OracleAnnouncementTLV] {
      val arity: Int = 1
      val reads: String => OracleAnnouncementTLV = OracleAnnouncementTLV.fromHex
    }

  implicit val contractInfoReads: Read[ContractInfo] =
    new Read[ContractInfo] {
      val arity: Int = 1
      val reads: String => ContractInfo = ContractInfo.fromHex
    }

  implicit val contractInfoTLVReads: Read[ContractInfoTLV] =
    new Read[ContractInfoTLV] {
      val arity: Int = 1
      val reads: String => ContractInfoTLV = ContractInfoTLV.fromHex
    }

  implicit val blockStampReads: Read[BlockStamp] =
    new Read[BlockStamp] {
      val arity: Int = 1
      private val dateRe = """(\d4)-(\d2)-(\d2)""".r

      val reads: String => BlockStamp = {
        case dateRe(year, month, day) =>
          val time = ZonedDateTime.of(year.toInt,
                                      month.toInt,
                                      day.toInt,
                                      0,
                                      0,
                                      0,
                                      0,
                                      ZoneId.of("UTC"))
          BlockTime(time)
        case str => BlockStamp.fromString(str)
      }
    }

  implicit val psbtReads: Read[PSBT] =
    new Read[PSBT] {
      val arity: Int = 1

      val reads: String => PSBT = PSBT.fromString
    }

  implicit val txReads: Read[Transaction] = new Read[Transaction] {
    val arity: Int = 1

    val reads: String => Transaction = Transaction.fromHex
  }

  implicit val outPointsRead: Read[TransactionOutPoint] =
    new Read[TransactionOutPoint] {
      val arity: Int = 1

      val reads: String => TransactionOutPoint = TransactionOutPoint.fromHex
    }

  implicit val coinSelectionAlgoReads: Read[CoinSelectionAlgo] =
    new Read[CoinSelectionAlgo] {
      val arity: Int = 1

      val reads: String => CoinSelectionAlgo =
        CoinSelectionAlgo.fromString
    }

  implicit val schnorrSigReads: Read[SchnorrDigitalSignature] =
    new Read[SchnorrDigitalSignature] {
      override def arity: Int = 1

      override def reads: String => SchnorrDigitalSignature =
        str => SchnorrDigitalSignature.fromHex(str.trim)
    }

  implicit val partialSigReads: Read[PartialSignature] =
    new Read[PartialSignature] {
      override def arity: Int = 1

      override def reads: String => PartialSignature =
        PartialSignature.fromHex
    }

  implicit val addressLabelTagReads: Read[AddressLabelTag] =
    new Read[AddressLabelTag] {
      val arity: Int = 1

      val reads: String => AddressLabelTag = str => AddressLabelTag(str)
    }

  implicit val sha256DigestBEReads: Read[Sha256DigestBE] =
    new Read[Sha256DigestBE] {
      val arity: Int = 1

      val reads: String => Sha256DigestBE = Sha256DigestBE.fromHex
    }

  implicit val lockUnspentOutputParameterReads: Read[
    LockUnspentOutputParameter] =
    new Read[LockUnspentOutputParameter] {
      val arity: Int = 1

      val reads: String => LockUnspentOutputParameter =
        LockUnspentOutputParameter.fromJsonString
    }

  implicit val sha256DigestReads: Read[Sha256Digest] =
    new Read[Sha256Digest] {
      val arity: Int = 1

      val reads: String => Sha256Digest = Sha256Digest.fromHex
    }

  implicit val dlcOfferTLVReads: Read[DLCOfferTLV] = new Read[DLCOfferTLV] {
    override def arity: Int = 1

    override def reads: String => DLCOfferTLV = DLCOfferTLV.fromHex
  }

  implicit val lnMessageDLCOfferTLVReads: Read[LnMessage[DLCOfferTLV]] =
    new Read[LnMessage[DLCOfferTLV]] {
      override def arity: Int = 1

      override def reads: String => LnMessage[DLCOfferTLV] =
        LnMessageFactory(DLCOfferTLV).fromHex
    }

  implicit val dlcAcceptTLVReads: Read[DLCAcceptTLV] = new Read[DLCAcceptTLV] {
    override def arity: Int = 1

    override def reads: String => DLCAcceptTLV = DLCAcceptTLV.fromHex
  }

  implicit val lnMessageDLCAcceptTLVReads: Read[LnMessage[DLCAcceptTLV]] =
    new Read[LnMessage[DLCAcceptTLV]] {
      override def arity: Int = 1

      override def reads: String => LnMessage[DLCAcceptTLV] =
        LnMessageFactory(DLCAcceptTLV).fromHex
    }

  implicit val dlcSignTLVReads: Read[DLCSignTLV] = new Read[DLCSignTLV] {
    override def arity: Int = 1

    override def reads: String => DLCSignTLV = DLCSignTLV.fromHex
  }

  implicit val lnMessageSignTLVReads: Read[LnMessage[DLCSignTLV]] =
    new Read[LnMessage[DLCSignTLV]] {
      override def arity: Int = 1

      override def reads: String => LnMessage[DLCSignTLV] =
        LnMessageFactory(DLCSignTLV).fromHex
    }

  implicit val extPrivKeyReads: Read[ExtPrivateKey] = new Read[ExtPrivateKey] {
    override def arity: Int = 1

    override def reads: String => ExtPrivateKey = ExtPrivateKey.fromString
  }

  implicit val mnemonicCodeReads: Read[MnemonicCode] = new Read[MnemonicCode] {
    override def arity: Int = 1

    override def reads: String => MnemonicCode =
      str => {
        val words = str.split(' ')

        MnemonicCode.fromWords(words.toVector)
      }
  }
}
