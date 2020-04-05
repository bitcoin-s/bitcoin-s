package org.bitcoins

import org.bitcoins.appCommons.JsonSerializers
import org.bitcoins.core.crypto.{ECPrivateKey, ExtPublicKey, Sha256DigestBE}
import org.bitcoins.core.protocol.{BitcoinAddress, BlockStamp}
import org.bitcoins.core.currency.{Bitcoins, Satoshis}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.protocol.ptlc.PTLCMessage._
import org.bitcoins.core.psbt.PSBT
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import upickle.default._

package object picklers {

  import org.bitcoins.core.crypto.DoubleSha256DigestBE

  implicit val bitcoinAddressPickler: ReadWriter[BitcoinAddress] =
    readwriter[String]
      .bimap(_.value, BitcoinAddress.fromStringExn(_))

  implicit val bitcoinsPickler: ReadWriter[Bitcoins] =
    readwriter[Double].bimap(_.toBigDecimal.toDouble, Bitcoins(_))

  implicit val satoshisPickler: ReadWriter[Satoshis] =
    readwriter[Long].bimap(_.toLong, Satoshis.apply)

  implicit val ecPrivateKeyPickler: ReadWriter[ECPrivateKey] =
    readwriter[String].bimap(_.hex, ECPrivateKey.fromHex)

  implicit val sha256DigestBEPickler: ReadWriter[Sha256DigestBE] =
    readwriter[String].bimap(_.hex, Sha256DigestBE.fromHex)

  implicit val doubleSha256DigestBEPickler: ReadWriter[DoubleSha256DigestBE] =
    readwriter[String].bimap(_.hex, DoubleSha256DigestBE.fromHex)

  implicit val blockStampPickler: ReadWriter[BlockStamp] =
    readwriter[String].bimap(_.mkString, BlockStamp.fromString(_).get)

  implicit val uInt32Pickler: ReadWriter[UInt32] =
    readwriter[Long].bimap(_.toLong, long => UInt32(long))

  implicit val psbtPickler: ReadWriter[PSBT] =
    readwriter[String].bimap(_.base64, PSBT.fromString)

  implicit val transactionPickler: ReadWriter[Transaction] =
    readwriter[String].bimap(_.hex, Transaction.fromHex)

  implicit val satoshisPerVirtualBytePickler: ReadWriter[
    SatoshisPerVirtualByte] =
    readwriter[Long].bimap(_.currencyUnit.satoshis.toLong,
                           l => SatoshisPerVirtualByte(Satoshis(l)))

  implicit val extPubKeyPickler: ReadWriter[ExtPublicKey] =
    readwriter[String].bimap(_.toString, ExtPublicKey.fromString(_).get)

  implicit val ptlcInvoicePickler: ReadWriter[PTLCInvoice] =
    readwriter[String]
      .bimap(invoice => JsonSerializers.toJson(invoice).toString(),
             str => JsonSerializers.getPTLCInvoice(ujson.read(str).obj))

  implicit val ptlcAcceptPickler: ReadWriter[PTLCAccept] =
    readwriter[String]
      .bimap(accept => JsonSerializers.toJson(accept).toString(),
             str => JsonSerializers.getPTLCAccept(ujson.read(str).obj))

  implicit val ptlcRefundSignaturePickler: ReadWriter[PTLCRefundSignature] =
    readwriter[String]
      .bimap(refundSig => JsonSerializers.toJson(refundSig).toString(),
             str => JsonSerializers.getPTLCRefundSignature(ujson.read(str).obj))
}
