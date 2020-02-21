package org.bitcoins

import org.bitcoins.core.currency.{Bitcoins, Satoshis}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.protocol.{BitcoinAddress, BlockStamp}
import org.bitcoins.core.psbt.PSBT
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.dlc.DLCMessage._
import upickle.default._

package object picklers {
  import org.bitcoins.core.crypto.DoubleSha256DigestBE
  implicit val bitcoinAddressPickler: ReadWriter[BitcoinAddress] =
    readwriter[String]
      .bimap(_.value, BitcoinAddress.fromStringExn(_))

  implicit val bitcoinsPickler: ReadWriter[Bitcoins] =
    readwriter[Double].bimap(_.toBigDecimal.toDouble, Bitcoins(_))

  implicit val doubleSha256DigestBEPickler: ReadWriter[DoubleSha256DigestBE] =
    readwriter[String].bimap(_.hex, DoubleSha256DigestBE(_))

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

  implicit val dlcOfferPickler: ReadWriter[DLCOffer] =
    readwriter[String]
      .bimap(_.toJsonStr, str => DLCOffer.fromJson(ujson.read(str)))

  implicit val dlcAcceptPickler: ReadWriter[DLCAccept] =
    readwriter[String]
      .bimap(_.toJsonStr, str => DLCAccept.fromJson(ujson.read(str).obj))

  implicit val dlcSignPickler: ReadWriter[DLCSign] =
    readwriter[String]
      .bimap(_.toJsonStr, str => DLCSign.fromJson(ujson.read(str).obj))

  implicit val blockStampPickler: ReadWriter[BlockStamp] =
    readwriter[String].bimap(_.mkString, BlockStamp.fromString(_).get)

  implicit val psbtPickler: ReadWriter[PSBT] =
    readwriter[String].bimap(_.base64, PSBT.fromString)

  implicit val transactionPickler: ReadWriter[Transaction] =
    readwriter[String].bimap(_.hex, Transaction.fromHex)
}
