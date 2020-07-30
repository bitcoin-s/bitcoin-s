package org.bitcoins.commons.serializers

import org.bitcoins.commons.jsonmodels.wallet.CoinSelectionAlgo
import org.bitcoins.commons.jsonmodels.dlc.DLCMessage._
import org.bitcoins.commons.jsonmodels.sbclient.{Exchange, TradingPair}
import org.bitcoins.core.crypto.ExtPublicKey
import org.bitcoins.core.currency.{Bitcoins, Satoshis}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionOutPoint}
import org.bitcoins.core.protocol.{BitcoinAddress, BlockStamp}
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.core.psbt.PSBT
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.crypto.{SchnorrDigitalSignature, Sha256DigestBE}
import upickle.default._

object Picklers {

  import org.bitcoins.crypto.DoubleSha256DigestBE

  implicit val bitcoinAddressPickler: ReadWriter[BitcoinAddress] =
    readwriter[String]
      .bimap(_.value, BitcoinAddress.fromStringExn)

  implicit val bitcoinsPickler: ReadWriter[Bitcoins] =
    readwriter[Double].bimap(_.toBigDecimal.toDouble, Bitcoins(_))

  implicit val satoshisPickler: ReadWriter[Satoshis] =
    readwriter[Long].bimap(_.toLong, Satoshis.apply)

  implicit val sha256DigestBEPickler: ReadWriter[Sha256DigestBE] =
    readwriter[String].bimap(_.hex, Sha256DigestBE.fromHex)

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

  implicit val contractInfoPickler: ReadWriter[ContractInfo] =
    readwriter[String].bimap(_.hex, ContractInfo.fromHex)

  implicit val schnorrDigitalSignaturePickler: ReadWriter[
    SchnorrDigitalSignature] =
    readwriter[String].bimap(_.hex, SchnorrDigitalSignature.fromHex)

  implicit val partialSignaturePickler: ReadWriter[PartialSignature] =
    readwriter[String].bimap(_.hex, PartialSignature.fromHex)

  implicit val dlcOfferPickler: ReadWriter[DLCOffer] =
    readwriter[String]
      .bimap(_.toJsonStr, str => DLCOffer.fromJson(ujson.read(str)))

  implicit val dlcAcceptPickler: ReadWriter[DLCAccept] =
    readwriter[String]
      .bimap(_.toJsonStr, str => DLCAccept.fromJson(ujson.read(str).obj))

  implicit val dlcSignPickler: ReadWriter[DLCSign] =
    readwriter[String]
      .bimap(_.toJsonStr, str => DLCSign.fromJson(ujson.read(str).obj))

  implicit val dlcMutualCloseSigPickler: ReadWriter[DLCMutualCloseSig] =
    readwriter[String].bimap(
      _.toJsonStr,
      str => DLCMutualCloseSig.fromJson(ujson.read(str).obj))

  implicit val blockStampPickler: ReadWriter[BlockStamp] =
    readwriter[String].bimap(_.mkString, BlockStamp.fromString)

  implicit val psbtPickler: ReadWriter[PSBT] =
    readwriter[String].bimap(_.base64, PSBT.fromString)

  implicit val transactionPickler: ReadWriter[Transaction] =
    readwriter[String].bimap(_.hex, Transaction.fromHex)

  implicit val extPubKeyPickler: ReadWriter[ExtPublicKey] =
    readwriter[String].bimap(_.toString, ExtPublicKey.fromString(_).get)

  implicit val transactionOutPointPickler: ReadWriter[TransactionOutPoint] =
    readwriter[String].bimap(_.hex, TransactionOutPoint.fromHex)

  implicit val coinSelectionAlgoPickler: ReadWriter[CoinSelectionAlgo] =
    readwriter[String].bimap(_.toString, CoinSelectionAlgo.fromString(_).get)

  implicit val exchangePickler: ReadWriter[Exchange] =
    readwriter[String].bimap(_.toLongString, Exchange.fromString(_).get)

  implicit val tradingPairPickler: ReadWriter[TradingPair] =
    readwriter[String].bimap(_.toString, TradingPair.fromString)
}
