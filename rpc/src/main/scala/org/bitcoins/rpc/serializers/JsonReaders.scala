package org.bitcoins.rpc.serializers

import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.currency.Bitcoins
import play.api.libs.json._

object JsonReaders {
  implicit object DoubleSha256DigestReads extends Reads[DoubleSha256Digest] {
    def reads(json: JsValue) = json match {
      case JsString(s) => JsSuccess(DoubleSha256Digest.fromHex(s))
      case err => JsError(s"error.expected.jsstring, got ${Json.toJson(err).toString()}")
    }
  }

  implicit object BitcoinsReads extends Reads[Bitcoins] {
    def reads(json: JsValue) = json match {
      case JsNumber(n) => JsSuccess(Bitcoins(n))
      case err => JsError(s"error.expected.jsnumber, got ${Json.toJson(err).toString()}")
    }
  }
}