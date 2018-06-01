package org.bitcoins.rpc.serializers

import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.script.ScriptPubKey
import play.api.libs.json._

object JsonWriters {
  implicit object BitcoinsWrites extends Writes[Bitcoins] {
    def writes(o: Bitcoins) = JsNumber(o.toBigDecimal)
  }

  implicit object BitcoinAddressWrites extends Writes[BitcoinAddress] {
    def writes(o: BitcoinAddress) = JsString(o.value)
  }

  implicit object DoubleSha256DigestWrites extends Writes[DoubleSha256Digest] {
    def writes(o: DoubleSha256Digest) = JsString(o.hex)
  }

  implicit object ScriptPubKeyWrites extends Writes[ScriptPubKey] {
    def writes(o: ScriptPubKey) = JsString(o.hex)
  }
}
