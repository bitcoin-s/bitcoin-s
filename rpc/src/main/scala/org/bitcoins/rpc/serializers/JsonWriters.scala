package org.bitcoins.rpc.serializers

import org.bitcoins.core.crypto.{DoubleSha256Digest, ECPrivateKey}
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.transaction.TransactionInput
import play.api.libs.json._

object JsonWriters {
  implicit object BitcoinsWrites extends Writes[Bitcoins] {
    override def writes(o: Bitcoins): JsValue = JsNumber(o.toBigDecimal)
  }

  implicit object BitcoinAddressWrites extends Writes[BitcoinAddress] {
    override def writes(o: BitcoinAddress): JsValue = JsString(o.value)
  }

  implicit object DoubleSha256DigestWrites extends Writes[DoubleSha256Digest] {
    override def writes(o: DoubleSha256Digest): JsValue = JsString(o.hex)
  }

  implicit object ScriptPubKeyWrites extends Writes[ScriptPubKey] {
    override def writes(o: ScriptPubKey): JsValue = JsString(o.hex)
  }

  implicit object TransactionInputWrites extends Writes[TransactionInput] {
    override def writes(o: TransactionInput): JsValue =
      JsObject(
        Seq(("txid", JsString(o.previousOutput.txId.hex)),
            ("vout", JsNumber(o.previousOutput.vout.toLong)),
            ("sequence", JsNumber(o.sequence.toLong))))
  }

  implicit object UInt32Writes extends Writes[UInt32] {
    override def writes(o: UInt32): JsValue = JsNumber(o.toLong)
  }
}
