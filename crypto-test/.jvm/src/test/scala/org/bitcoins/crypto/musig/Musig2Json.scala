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
  MuSigNoncePubReads,
  SchnorrDigitalSignatureReads
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

  // Nonce aggregation test vectors
  case class NonceAggValidTest(
      pnonce_indices: Vector[Int],
      expected: MuSigNoncePub,
      comment: Option[String]
  )

  case class NonceAggErrorTest(
      pnonce_indices: Vector[Int],
      error: KeyAggError,
      comment: Option[String]
  )

  case class NonceAggVectors(
      pnonces: Vector[ByteVector],
      valid_test_cases: Vector[NonceAggValidTest],
      error_test_cases: Vector[NonceAggErrorTest]
  )

  implicit val nonceAggValidTestReads: Reads[NonceAggValidTest] =
    Json.reads[NonceAggValidTest]
  implicit val nonceAggErrorTestReads: Reads[NonceAggErrorTest] =
    Json.reads[NonceAggErrorTest]
  implicit val nonceAggVectorsReads: Reads[NonceAggVectors] =
    Json.reads[NonceAggVectors]

  // Tweak vectors test structures
  case class TweakValidTest(
      key_indices: Vector[Int],
      nonce_indices: Vector[Int],
      tweak_indices: Vector[Int],
      is_xonly: Vector[Boolean],
      signer_index: Int,
      expected: FieldElement,
      comment: Option[String]
  )

  case class TweakErrorTest(
      key_indices: Vector[Int],
      nonce_indices: Vector[Int],
      tweak_indices: Vector[Int],
      is_xonly: Vector[Boolean],
      signer_index: Int,
      error: KeyAggError,
      comment: Option[String]
  )

  case class TweakVectors(
      sk: ByteVector,
      pubkeys: Vector[ECPublicKeyBytes],
      secnonce: MuSigNoncePriv,
      pnonces: Vector[MuSigNoncePub],
      aggnonce: MuSigNoncePub,
      tweaks: Vector[ByteVector],
      msg: ByteVector,
      valid_test_cases: Vector[TweakValidTest],
      error_test_cases: Vector[TweakErrorTest]
  )

  implicit val tweakValidTestReads: Reads[TweakValidTest] =
    Json.reads[TweakValidTest]
  implicit val tweakErrorTestReads: Reads[TweakErrorTest] =
    Json.reads[TweakErrorTest]
  implicit val tweakVectorsReads: Reads[TweakVectors] = Json.reads[TweakVectors]

  // Sign / verify vectors
  case class SignValidTest(
      key_indices: Vector[Int],
      nonce_indices: Vector[Int],
      aggnonce_index: Int,
      msg_index: Int,
      signer_index: Int,
      expected: FieldElement,
      comment: Option[String]
  )

  case class SignErrorTest(
      key_indices: Vector[Int],
      aggnonce_index: Int,
      msg_index: Int,
      secnonce_index: Option[Int],
      error: KeyAggError,
      comment: Option[String]
  )

  case class VerifyFailTest(
      sig: FieldElement,
      key_indices: Vector[Int],
      nonce_indices: Vector[Int],
      msg_index: Int,
      signer_index: Int,
      comment: Option[String]
  )

  case class VerifyErrorTest(
      sig: ByteVector,
      key_indices: Vector[Int],
      nonce_indices: Vector[Int],
      msg_index: Int,
      signer_index: Int,
      error: KeyAggError,
      comment: Option[String]
  )

  case class SignVerifyVectors(
      sk: ByteVector,
      pubkeys: Vector[ECPublicKeyBytes],
      secnonces: Vector[ByteVector],
      pnonces: Vector[ByteVector],
      aggnonces: Vector[ByteVector],
      msgs: Vector[ByteVector],
      valid_test_cases: Vector[SignValidTest],
      sign_error_test_cases: Vector[SignErrorTest],
      verify_fail_test_cases: Vector[VerifyFailTest],
      verify_error_test_cases: Vector[VerifyErrorTest]
  )

  implicit val signValidTestReads: Reads[SignValidTest] =
    Json.reads[SignValidTest]
  implicit val signErrorTestReads: Reads[SignErrorTest] =
    Json.reads[SignErrorTest]
  implicit val verifyFailTestReads: Reads[VerifyFailTest] =
    Json.reads[VerifyFailTest]
  implicit val verifyErrorTestReads: Reads[VerifyErrorTest] =
    Json.reads[VerifyErrorTest]
  implicit val signVerifyVectorsReads: Reads[SignVerifyVectors] =
    Json.reads[SignVerifyVectors]

  // --- sig_agg_vectors.json structures ---
  case class SigAggValidTest(
      aggnonce: ByteVector,
      nonce_indices: Vector[Int],
      key_indices: Vector[Int],
      tweak_indices: Vector[Int],
      is_xonly: Vector[Boolean],
      psig_indices: Vector[Int],
      expected: SchnorrDigitalSignature,
      comment: Option[String]
  )

  case class SigAggErrorTest(
      aggnonce: ByteVector,
      nonce_indices: Vector[Int],
      key_indices: Vector[Int],
      tweak_indices: Vector[Int],
      is_xonly: Vector[Boolean],
      psig_indices: Vector[Int],
      error: KeyAggError,
      comment: Option[String]
  )

  case class SigAggVectors(
      pubkeys: Vector[ECPublicKeyBytes],
      pnonces: Vector[ByteVector],
      tweaks: Vector[ByteVector],
      psigs: Vector[ByteVector],
      msg: ByteVector,
      valid_test_cases: Vector[SigAggValidTest],
      error_test_cases: Vector[SigAggErrorTest]
  )

  implicit val sigAggValidTestReads: Reads[SigAggValidTest] =
    Json.reads[SigAggValidTest]
  implicit val sigAggErrorTestReads: Reads[SigAggErrorTest] =
    Json.reads[SigAggErrorTest]
  implicit val sigAggVectorsReads: Reads[SigAggVectors] =
    Json.reads[SigAggVectors]

  // --- det_sign_vectors.json structures ---
  case class DetSignValidTest(
      rand: Option[ByteVector],
      aggothernonce: ByteVector,
      key_indices: Vector[Int],
      tweaks: Vector[ByteVector],
      is_xonly: Vector[Boolean],
      msg_index: Int,
      signer_index: Int,
      expected: Vector[ByteVector],
      comment: Option[String]
  )

  case class DetSignErrorTest(
      rand: Option[ByteVector],
      aggothernonce: ByteVector,
      key_indices: Vector[Int],
      tweaks: Vector[ByteVector],
      is_xonly: Vector[Boolean],
      msg_index: Int,
      signer_index: Int,
      error: KeyAggError,
      comment: Option[String]
  )

  case class DetSignVectors(
      sk: ByteVector,
      pubkeys: Vector[ECPublicKeyBytes],
      msgs: Vector[ByteVector],
      valid_test_cases: Vector[DetSignValidTest],
      error_test_cases: Vector[DetSignErrorTest]
  )

  implicit val detSignValidTestReads: Reads[DetSignValidTest] =
    Json.reads[DetSignValidTest]
  implicit val detSignErrorTestReads: Reads[DetSignErrorTest] =
    Json.reads[DetSignErrorTest]
  implicit val detSignVectorsReads: Reads[DetSignVectors] =
    Json.reads[DetSignVectors]
}
