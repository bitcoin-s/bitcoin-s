package org.bitcoins.rpc.serializers

import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.protocol.BitcoinAddress
import play.api.libs.json._

object JsonWriters {
  implicit object BitcoinsWrites extends Writes[Bitcoins] {
    def writes(o: Bitcoins) = JsNumber(o.toBigDecimal)
  }

  implicit object BitcoinAddressWrites extends Writes[BitcoinAddress] {
    def writes(o: BitcoinAddress) = JsString(o.value)
  }
}
