package org.bitcoins

import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.crypto.DoubleSha256DigestBE
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

}
