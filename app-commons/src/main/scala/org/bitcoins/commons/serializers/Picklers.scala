package org.bitcoins.commons.serializers

import org.bitcoins.commons.jsonmodels.bitcoind.RpcOpts.LockUnspentOutputParameter
import org.bitcoins.commons.jsonmodels.dlc.DLCMessage._
import org.bitcoins.core.api.wallet.CoinSelectionAlgo
import org.bitcoins.core.crypto.ExtPublicKey
import org.bitcoins.core.currency.{Bitcoins, Satoshis}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionOutPoint}
import org.bitcoins.core.protocol.{BitcoinAddress, BlockStamp}
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.core.psbt.PSBT
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.core.wallet.utxo.AddressLabelTag
import org.bitcoins.crypto._
import scodec.bits.ByteVector
import upickle.default._

import java.io.File
import java.nio.file.Path
import java.time.Instant

object Picklers {

  implicit val pathPickler: ReadWriter[Path] =
    readwriter[String].bimap(_.toString, str => new File(str).toPath)

  implicit val byteVectorPickler: ReadWriter[ByteVector] =
    readwriter[String].bimap(_.toHex, str => ByteVector.fromValidHex(str))

  implicit val bitcoinAddressPickler: ReadWriter[BitcoinAddress] =
    readwriter[String]
      .bimap(_.value, BitcoinAddress.fromString)

  implicit val bitcoinsPickler: ReadWriter[Bitcoins] =
    readwriter[Double].bimap(_.toBigDecimal.toDouble, Bitcoins(_))

  implicit val satoshisPickler: ReadWriter[Satoshis] =
    readwriter[Long].bimap(_.toLong, Satoshis.apply)

  implicit val schnorrNoncePickler: ReadWriter[SchnorrNonce] =
    readwriter[String].bimap(_.hex, SchnorrNonce.fromHex)

  implicit val enumEventDescriptorPickler: ReadWriter[
    EnumEventDescriptorV0TLV] =
    readwriter[String].bimap(_.hex, EnumEventDescriptorV0TLV.fromHex)

  implicit val rangeEventDescriptorPickler: ReadWriter[
    RangeEventDescriptorV0TLV] =
    readwriter[String].bimap(_.hex, RangeEventDescriptorV0TLV.fromHex)

  implicit val digitDecompEventDescriptorPickler: ReadWriter[
    DigitDecompositionEventDescriptorV0TLV] =
    readwriter[String].bimap(_.hex,
                             DigitDecompositionEventDescriptorV0TLV.fromHex)

  implicit val eventDescriptorPickler: ReadWriter[EventDescriptorTLV] =
    readwriter[String].bimap(_.hex, EventDescriptorTLV.fromHex)

  implicit val oracleEventVoPickler: ReadWriter[OracleEventV0TLV] =
    readwriter[String].bimap(_.hex, OracleEventV0TLV.fromHex)

  implicit val instantPickler: ReadWriter[Instant] =
    readwriter[Long].bimap(_.getEpochSecond, Instant.ofEpochSecond)

  implicit val aesPasswordPickler: ReadWriter[AesPassword] =
    readwriter[String].bimap(_.toStringSensitive, AesPassword.fromString)

  implicit val sha256DigestBEPickler: ReadWriter[Sha256DigestBE] =
    readwriter[String].bimap(_.hex, Sha256DigestBE.fromHex)

  implicit val sha256DigestPickler: ReadWriter[Sha256Digest] =
    readwriter[String].bimap(_.hex, Sha256Digest.fromHex)

  implicit val doubleSha256DigestBEPickler: ReadWriter[DoubleSha256DigestBE] =
    readwriter[String].bimap(_.hex, DoubleSha256DigestBE.fromHex)

  implicit val uInt32Pickler: ReadWriter[UInt32] =
    readwriter[Long].bimap(_.toLong, long => UInt32(long))

  implicit val satoshisPerVirtualBytePickler: ReadWriter[
    SatoshisPerVirtualByte] =
    readwriter[Long]
      .bimap(_.toLong, long => SatoshisPerVirtualByte(Satoshis(long)))

  implicit val oracleInfoPickler: ReadWriter[OracleInfo] =
    readwriter[String].bimap(_.hex, OracleInfo.fromHex)

  implicit val oracleAnnouncementPickler: ReadWriter[OracleAnnouncementTLV] =
    readwriter[String].bimap(_.hex, OracleAnnouncementTLV.fromHex)

  implicit val contractInfoPickler: ReadWriter[ContractInfo] =
    readwriter[String].bimap(_.hex, ContractInfo.fromHex)

  implicit val contractInfoTLVPickler: ReadWriter[ContractInfoTLV] =
    readwriter[String].bimap(_.hex, ContractInfoTLV.fromHex)

  implicit val schnorrDigitalSignaturePickler: ReadWriter[
    SchnorrDigitalSignature] =
    readwriter[String].bimap(_.hex, SchnorrDigitalSignature.fromHex)

  implicit val partialSignaturePickler: ReadWriter[PartialSignature] =
    readwriter[String].bimap(_.hex, PartialSignature.fromHex)

  implicit val dlcOfferTLVPickler: ReadWriter[DLCOfferTLV] =
    readwriter[String].bimap(_.hex, DLCOfferTLV.fromHex)

  implicit val lnMessageDLCOfferTLVPickler: ReadWriter[LnMessage[DLCOfferTLV]] =
    readwriter[String].bimap(_.hex, LnMessageFactory(DLCOfferTLV).fromHex)

  implicit val dlcAcceptTLVPickler: ReadWriter[DLCAcceptTLV] =
    readwriter[String].bimap(_.hex, DLCAcceptTLV.fromHex)

  implicit val lnMessageDLCAcceptTLVPickler: ReadWriter[
    LnMessage[DLCAcceptTLV]] =
    readwriter[String].bimap(_.hex, LnMessageFactory(DLCAcceptTLV).fromHex)

  implicit val dlcSignTLVPickler: ReadWriter[DLCSignTLV] =
    readwriter[String].bimap(_.hex, DLCSignTLV.fromHex)

  implicit val lnMessageDLCSignTLVPickler: ReadWriter[LnMessage[DLCSignTLV]] =
    readwriter[String].bimap(_.hex, LnMessageFactory(DLCSignTLV).fromHex)

  implicit val blockStampPickler: ReadWriter[BlockStamp] =
    readwriter[String].bimap(_.mkString, BlockStamp.fromString)

  implicit val psbtPickler: ReadWriter[PSBT] =
    readwriter[String].bimap(_.base64, PSBT.fromString)

  implicit val transactionPickler: ReadWriter[Transaction] =
    readwriter[String].bimap(_.hex, Transaction.fromHex)

  implicit val extPubKeyPickler: ReadWriter[ExtPublicKey] =
    readwriter[String].bimap(_.toString, ExtPublicKey.fromString)

  implicit val transactionOutPointPickler: ReadWriter[TransactionOutPoint] =
    readwriter[String].bimap(_.hex, TransactionOutPoint.fromHex)

  implicit val coinSelectionAlgoPickler: ReadWriter[CoinSelectionAlgo] =
    readwriter[String].bimap(_.toString, CoinSelectionAlgo.fromString)

  implicit val addressLabelTagPickler: ReadWriter[AddressLabelTag] =
    readwriter[String].bimap(_.name, AddressLabelTag)

  implicit val lockUnspentOutputParameterPickler: ReadWriter[
    LockUnspentOutputParameter] =
    readwriter[String].bimap(_.toJson.render(),
                             LockUnspentOutputParameter.fromJsonString)
}
