package org.bitcoins

import org.bitcoins.core.protocol.{BitcoinAddress, BlockStamp}
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.psbt.PSBT
import upickle.default._

package object picklers {
  import org.bitcoins.core.crypto.DoubleSha256DigestBE
  implicit val bitcoinAddressPickler: ReadWriter[BitcoinAddress] =
    readwriter[String]
      .bimap(_.value, BitcoinAddress.fromStringExn(_))

  implicit val bitcoinsPickler: ReadWriter[Bitcoins] =
    readwriter[Double].bimap(_.toBigDecimal.toDouble, Bitcoins(_))

  implicit val doubleSha256DigestBEPickler: ReadWriter[DoubleSha256DigestBE] =
    readwriter[String].bimap(_.hex, DoubleSha256DigestBE.fromHex)

  implicit val blockStampPickler: ReadWriter[BlockStamp] =
    readwriter[String].bimap(_.mkString, BlockStamp.fromString(_).get)

  implicit val psbtPickler: ReadWriter[PSBT] =
    readwriter[String].bimap(_.base64, PSBT.fromString)

  implicit val transactionPickler: ReadWriter[Transaction] =
    readwriter[String].bimap(_.hex, Transaction.fromHex)
}
