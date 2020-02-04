package org.bitcoins

import org.bitcoins.core.crypto.Sha256DigestBE
import org.bitcoins.core.protocol.{BitcoinAddress, BlockStamp}
import org.bitcoins.core.currency.{Bitcoins, Satoshis}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.dlc.DLCMessage.{DLCAccept, DLCOffer, OracleInfo}
import upickle.default._

package object picklers {
  import org.bitcoins.core.crypto.DoubleSha256DigestBE
  implicit val bitcoinAddressPickler: ReadWriter[BitcoinAddress] =
    readwriter[String]
      .bimap(_.value, BitcoinAddress.fromStringExn(_))

  implicit val bitcoinsPickler: ReadWriter[Bitcoins] =
    readwriter[Double].bimap(_.toBigDecimal.toDouble, Bitcoins(_))

  implicit val doubleSha256DigestBEPickler: ReadWriter[DoubleSha256DigestBE] =
    readwriter[String].bimap(_.hex, hex => DoubleSha256DigestBE(hex))

  implicit val sha256DigestBEPickler: ReadWriter[Sha256DigestBE] =
    readwriter[String].bimap(_.hex, hex => Sha256DigestBE(hex))

  implicit val uInt32Pickler: ReadWriter[UInt32] =
    readwriter[Long].bimap(_.toLong, long => UInt32(long))

  implicit val satoshisPerVirtualByteReader: ReadWriter[
    SatoshisPerVirtualByte] =
    readwriter[Long]
      .bimap(_.toLong, long => SatoshisPerVirtualByte(Satoshis(long)))

  implicit val oracleInfoPickler: ReadWriter[OracleInfo] =
    readwriter[String].bimap(_.hex, hex => OracleInfo(hex))

  implicit val dlcOfferPickler: ReadWriter[DLCOffer] =
    readwriter[String]
      .bimap(_.toJsonStr, str => DLCOffer.fromJson(ujson.read(str).obj))

  implicit val dlcAcceptPickler: ReadWriter[DLCAccept] =
    readwriter[String]
      .bimap(_.toJsonStr, str => DLCAccept.fromJson(ujson.read(str).obj))

  implicit val blockStampPickler: ReadWriter[BlockStamp] =
    readwriter[String].bimap(_.mkString, str => BlockStamp.fromString(str).get)
}
