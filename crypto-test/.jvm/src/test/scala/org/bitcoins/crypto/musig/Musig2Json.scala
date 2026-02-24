package org.bitcoins.crypto.musig

import org.bitcoins.crypto.*
import play.api.libs.json.*
import org.bitcoins.commons.serializers.JsonReaders.{
  ECPublicKeyBytesReads,
  SchnorrPublicKeyReads,
  byteVectorReads,
  ECPublicKeyReads,
  FieldElementReads,
  MuSigNoncePrivReads,
  MuSigNoncePubReads
}
import scodec.bits.ByteVector

/** Test vector case classes and JSON parsing for musig2 key aggregation
  * vectors. Uses existing serializers from `app-commons` (JsonSerializers) for
  * crypto types.
  */
object Musig2Json {
  case class ValidKeyAggTest(
      key_indices: Vector[Int],
      expected: SchnorrPublicKey)

  case class KeyAggError(
      `type`: String,
      signer: Option[Int],
      contrib: Option[String],
      message: Option[String]
  )

  case class ErrorKeyAggTest(
      key_indices: Vector[Int],
      tweak_indices: Vector[Int],
      is_xonly: Vector[Boolean],
      error: KeyAggError,
      comment: Option[String]
  )

  case class KeyAggVectors(
      pubkeys: Vector[ECPublicKeyBytes],
      tweaks: Vector[ByteVector],
      valid_test_cases: Vector[ValidKeyAggTest],
      error_test_cases: Vector[ErrorKeyAggTest]
  )

  implicit val validKeyAggTestReads: Reads[ValidKeyAggTest] = {
    Json.reads[ValidKeyAggTest]
  }
  implicit val keyAggErrorReads: Reads[KeyAggError] = Json.reads[KeyAggError]
  implicit val errorKeyAggTestReads: Reads[ErrorKeyAggTest] =
    Json.reads[ErrorKeyAggTest]

  implicit val keyAggVectorsReads: Reads[KeyAggVectors] = {
    Json.reads[KeyAggVectors]
  }

  // Nonce generation test vectors
  case class NonceGenTestCase(
      rand_ : ByteVector,
      sk: Option[FieldElement],
      pk: ECPublicKey,
      aggpk: Option[SchnorrPublicKey],
      msg: Option[ByteVector],
      extra_in: Option[ByteVector],
      expected_secnonce: MuSigNoncePriv,
      expected_pubnonce: MuSigNoncePub
  )

  case class NonceGenVectors(test_cases: Vector[NonceGenTestCase])

  implicit val nonceGenTestCaseReads: Reads[NonceGenTestCase] =
    Json.reads[NonceGenTestCase]
  implicit val nonceGenVectorsReads: Reads[NonceGenVectors] =
    Json.reads[NonceGenVectors]
}
