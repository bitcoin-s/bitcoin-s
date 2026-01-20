package org.bitcoins.crypto.frost
import play.api.libs.json.*
import play.api.libs.functional.syntax.*
import scodec.bits.ByteVector
import org.bitcoins.commons.serializers.JsonReaders.{
  byteVectorReads,
  ECPublicKeyReads,
  XOnlyPubKeyReads
}
import org.bitcoins.crypto.{ECPublicKey, XOnlyPubKey}
object FrostJson {

  case class NonceGenTestVectors(test_cases: Vector[NonceGenTestVector])
  case class NonceGenTestVector(
      rand: ByteVector,
      secshare: Option[ByteVector],
      pubshare: Option[ECPublicKey],
      threshold_pubkey: Option[XOnlyPubKey],
      msg: Option[ByteVector],
      extra_in: Option[ByteVector],
      expected_secnonce: ByteVector,
      expected_pubnonce: ByteVector,
      comment: String
  )
  implicit val nonceGenTestVectorReads: Reads[NonceGenTestVector] = (
    (__ \ "rand_").read[ByteVector] and
      (__ \ "secshare").readNullable[ByteVector] and
      (__ \ "pubshare").readNullable[ECPublicKey] and
      (__ \ "threshold_pubkey").readNullable[XOnlyPubKey] and
      (__ \ "msg").readNullable[ByteVector] and
      (__ \ "extra_in").readNullable[ByteVector] and
      (__ \ "expected_secnonce").read[ByteVector] and
      (__ \ "expected_pubnonce").read[ByteVector] and
      (__ \ "comment").read[String]
  )(NonceGenTestVector.apply _)
  implicit val nonceGenTestVectorsReads: Reads[NonceGenTestVectors] =
    Json.reads[NonceGenTestVectors]
}
