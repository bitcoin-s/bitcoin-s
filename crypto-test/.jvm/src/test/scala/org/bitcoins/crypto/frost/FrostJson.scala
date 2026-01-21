package org.bitcoins.crypto.frost
import play.api.libs.json.*
import play.api.libs.functional.syntax.*
import scodec.bits.ByteVector
import org.bitcoins.commons.serializers.JsonReaders.{
  byteVectorReads,
  ECPublicKeyReads,
  XOnlyPubKeyReads,
  FrostNonceReads
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
      expected_pubnonce: FrostNonce,
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
      (__ \ "expected_pubnonce").read[FrostNonce] and
      (__ \ "comment").read[String]
  )(NonceGenTestVector.apply _)
  implicit val nonceGenTestVectorsReads: Reads[NonceGenTestVectors] =
    Json.reads[NonceGenTestVectors]

  // --- Nonce aggregation vectors ---
  case class NonceAggTestVectors(
      pubnonces: Vector[ByteVector],
      valid_test_cases: Vector[NonceAggValidTestCase],
      error_test_cases: Vector[NonceAggErrorTestCase]
  )

  case class NonceAggValidTestCase(
      pubnonce_indices: Vector[Int],
      participant_identifiers: Vector[Int],
      expected_aggnonce: FrostNonce,
      comment: Option[String]
  )

  case class NonceAggError(
      `type`: String,
      id: Int,
      contrib: String
  )

  case class NonceAggErrorTestCase(
      pubnonce_indices: Vector[Int],
      participant_identifiers: Vector[Int],
      error: NonceAggError,
      comment: Option[String]
  )

  implicit val nonceAggValidReads: Reads[NonceAggValidTestCase] = (
    (__ \ "pubnonce_indices").read[Seq[Int]].map(_.toVector) and
      (__ \ "participant_identifiers").read[Seq[Int]].map(_.toVector) and
      (__ \ "expected_aggnonce").read[FrostNonce] and
      (__ \ "comment").readNullable[String]
  )(NonceAggValidTestCase.apply _)

  implicit val nonceAggErrorReads: Reads[NonceAggError] = (
    (__ \ "type").read[String] and
      (__ \ "id").read[Int] and
      (__ \ "contrib").read[String]
  )(NonceAggError.apply _)

  implicit val nonceAggErrorTestCaseReads: Reads[NonceAggErrorTestCase] = (
    (__ \ "pubnonce_indices").read[Seq[Int]].map(_.toVector) and
      (__ \ "participant_identifiers").read[Seq[Int]].map(_.toVector) and
      (__ \ "error").read[NonceAggError] and
      (__ \ "comment").readNullable[String]
  )(NonceAggErrorTestCase.apply _)

  implicit val nonceAggTestVectorsReads: Reads[NonceAggTestVectors] = (
    (__ \ "pubnonces").read[Seq[ByteVector]].map(_.toVector) and
      (__ \ "valid_test_cases")
        .read[Seq[NonceAggValidTestCase]]
        .map(_.toVector) and
      (__ \ "error_test_cases").read[Seq[NonceAggErrorTestCase]].map(_.toVector)
  )(NonceAggTestVectors.apply _)
}
