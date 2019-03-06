package org.bitcoins.rpc.serializers

import org.bitcoins.core.crypto.{DoubleSha256Digest, DoubleSha256DigestBE}
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.ln.currency.MilliSatoshis
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionInput}
import org.bitcoins.core.util.BitcoinSUtil
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

  implicit object DoubleSha256DigestBEWrites
      extends Writes[DoubleSha256DigestBE] {
    override def writes(o: DoubleSha256DigestBE): JsValue = JsString(o.hex)
  }

  implicit object ScriptPubKeyWrites extends Writes[ScriptPubKey] {
    override def writes(o: ScriptPubKey): JsValue =
      JsString(BitcoinSUtil.encodeHex(o.asmBytes))
  }

  implicit object TransactionInputWrites extends Writes[TransactionInput] {
    override def writes(o: TransactionInput): JsValue =
      JsObject(
        Seq(("txid", JsString(o.previousOutput.txIdBE.hex)),
            ("vout", JsNumber(o.previousOutput.vout.toLong)),
            ("sequence", JsNumber(o.sequence.toLong))))
  }

  implicit object UInt32Writes extends Writes[UInt32] {
    override def writes(o: UInt32): JsValue = JsNumber(o.toLong)
  }

  implicit object TransactionWrites extends Writes[Transaction] {
    override def writes(o: Transaction): JsValue = JsString(o.hex)
  }

  implicit def mapWrites[K, V](keyString: K => String)(
      implicit
      vWrites: Writes[V]): Writes[Map[K, V]] = new Writes[Map[K, V]] {
    override def writes(o: Map[K, V]): JsValue = {
      Json.toJson(o.map { case (k, v) => (keyString(k), v) })
    }
  }

  implicit object MilliSatoshisWrites extends Writes[MilliSatoshis] {
    override def writes(o: MilliSatoshis): JsValue = JsNumber(o.toBigDecimal)
  }
}
