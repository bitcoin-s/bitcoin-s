package org.bitcoins.crypto.frost
import play.api.libs.json.*
import play.api.libs.functional.syntax.*
import scodec.bits.ByteVector
import org.bitcoins.commons.serializers.JsonReaders.{
  ECPublicKeyReads,
  FrostNoncePrivReads,
  FrostNoncePubReads,
  XOnlyPubKeyReads,
  FieldElementReads
}
import org.bitcoins.crypto.{ECPublicKey, FieldElement, XOnlyPubKey}
object FrostJson {

  // Local ByteVector reader (accepts empty string as empty ByteVector).
  // Placed here so implicit Reads[ByteVector] is available for all subsequent Reads.
  implicit val byteVectorReadsLocal: Reads[ByteVector] = Reads {
    case JsString(s) =>
      try {
        if (s.isEmpty) JsSuccess(ByteVector.empty)
        else {
          ByteVector.fromHex(s) match {
            case Some(bv) => JsSuccess(bv)
            case None     => JsError("Invalid hex for ByteVector")
          }
        }
      } catch {
        case _: Throwable => JsError("Invalid hex for ByteVector")
      }
    case _ => JsError("Expected JSON string for ByteVector")
  }

  case class NonceGenTestVectors(test_cases: Vector[NonceGenTestVector])
  case class NonceGenTestVector(
      rand: ByteVector,
      secshare: Option[ByteVector],
      pubshare: Option[ECPublicKey],
      threshold_pubkey: Option[XOnlyPubKey],
      msg: Option[ByteVector],
      extra_in: Option[ByteVector],
      expected_secnonce: FrostNoncePriv,
      expected_pubnonce: FrostNoncePub,
      comment: String
  )
  implicit val nonceGenTestVectorReads: Reads[NonceGenTestVector] = (
    (__ \ "rand_").read[ByteVector] and
      (__ \ "secshare").readNullable[ByteVector] and
      (__ \ "pubshare").readNullable[ECPublicKey] and
      (__ \ "threshold_pubkey").readNullable[XOnlyPubKey] and
      (__ \ "msg").readNullable[ByteVector] and
      (__ \ "extra_in").readNullable[ByteVector] and
      (__ \ "expected_secnonce").read[FrostNoncePriv] and
      (__ \ "expected_pubnonce").read[FrostNoncePub] and
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
      expected_aggnonce: FrostNoncePub,
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
      (__ \ "expected_aggnonce").read[FrostNoncePub] and
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

  // --- Sign / Verify vectors ---
  case class SignVerifyVectors(
      n: Int,
      t: Int,
      threshold_pubkey: ECPublicKey,
      secshare_p0: ByteVector,
      identifiers: Vector[Int],
      pubshares: Vector[ByteVector],
      secnonces_p0: Vector[FrostNoncePriv],
      pubnonces: Vector[ByteVector],
      aggnonces: Vector[ByteVector],
      msgs: Vector[ByteVector],
      valid_test_cases: Vector[SignValidTestCase],
      sign_error_test_cases: Vector[SignErrorTestCase],
      verify_fail_test_cases: Vector[VerifyFailTestCase],
      verify_error_test_cases: Vector[VerifyErrorTestCase]
  )

  case class SignValidTestCase(
      id_indices: Vector[Int],
      pubshare_indices: Vector[Int],
      pubnonce_indices: Vector[Int],
      aggnonce_index: Int,
      msg_index: Int,
      signer_index: Int,
      expected: ByteVector,
      comment: Option[String]
  )

  case class SignError(
      `type`: String,
      message: Option[String],
      id: Option[Int],
      contrib: Option[String])

  case class SignErrorTestCase(
      id_indices: Vector[Int],
      pubshare_indices: Vector[Int],
      aggnonce_index: Int,
      msg_index: Int,
      signer_index: Option[Int],
      signer_id: Option[Int],
      secnonce_index: Option[Int],
      error: SignError,
      comment: Option[String]
  )

  case class VerifyFailTestCase(
      psig: ByteVector,
      id_indices: Vector[Int],
      pubshare_indices: Vector[Int],
      pubnonce_indices: Vector[Int],
      msg_index: Int,
      signer_index: Int,
      comment: Option[String]
  )

  case class VerifyErrorTestCase(
      psig: ByteVector,
      id_indices: Vector[Int],
      pubshare_indices: Vector[Int],
      pubnonce_indices: Vector[Int],
      msg_index: Int,
      signer_index: Int,
      error: SignError,
      comment: Option[String]
  )

  implicit val signValidReads: Reads[SignValidTestCase] = (
    (__ \ "id_indices").read[Seq[Int]].map(_.toVector) and
      (__ \ "pubshare_indices").read[Seq[Int]].map(_.toVector) and
      (__ \ "pubnonce_indices").read[Seq[Int]].map(_.toVector) and
      (__ \ "aggnonce_index").read[Int] and
      (__ \ "msg_index").read[Int] and
      (__ \ "signer_index").read[Int] and
      (__ \ "expected").read[ByteVector] and
      (__ \ "comment").readNullable[String]
  )(SignValidTestCase.apply _)

  implicit val signErrorReads: Reads[SignError] = (
    (__ \ "type").read[String] and
      (__ \ "message").readNullable[String] and
      (__ \ "id").readNullable[Int] and
      (__ \ "contrib").readNullable[String]
  )(SignError.apply _)

  implicit val signErrorTestCaseReads: Reads[SignErrorTestCase] = (
    (__ \ "id_indices").read[Seq[Int]].map(_.toVector) and
      (__ \ "pubshare_indices").read[Seq[Int]].map(_.toVector) and
      (__ \ "aggnonce_index").read[Int] and
      (__ \ "msg_index").read[Int] and
      (__ \ "signer_index").readNullable[Int] and
      (__ \ "signer_id").readNullable[Int] and
      (__ \ "secnonce_index").readNullable[Int] and
      (__ \ "error").read[SignError] and
      (__ \ "comment").readNullable[String]
  )(SignErrorTestCase.apply _)

  implicit val verifyFailReads: Reads[VerifyFailTestCase] = (
    (__ \ "psig").read[ByteVector] and
      (__ \ "id_indices").read[Seq[Int]].map(_.toVector) and
      (__ \ "pubshare_indices").read[Seq[Int]].map(_.toVector) and
      (__ \ "pubnonce_indices").read[Seq[Int]].map(_.toVector) and
      (__ \ "msg_index").read[Int] and
      (__ \ "signer_index").read[Int] and
      (__ \ "comment").readNullable[String]
  )(VerifyFailTestCase.apply _)

  implicit val verifyErrorReads: Reads[VerifyErrorTestCase] = (
    (__ \ "psig").read[ByteVector] and
      (__ \ "id_indices").read[Seq[Int]].map(_.toVector) and
      (__ \ "pubshare_indices").read[Seq[Int]].map(_.toVector) and
      (__ \ "pubnonce_indices").read[Seq[Int]].map(_.toVector) and
      (__ \ "msg_index").read[Int] and
      (__ \ "signer_index").read[Int] and
      (__ \ "error").read[SignError] and
      (__ \ "comment").readNullable[String]
  )(VerifyErrorTestCase.apply _)

  implicit val signVerifyVectorsReads: Reads[SignVerifyVectors] = (
    (__ \ "n").read[Int] and
      (__ \ "t").read[Int] and
      (__ \ "threshold_pubkey").read[ECPublicKey] and
      (__ \ "secshare_p0").read[ByteVector] and
      (__ \ "identifiers").read[Seq[Int]].map(_.toVector) and
      (__ \ "pubshares").read[Seq[ByteVector]].map(_.toVector) and
      (__ \ "secnonces_p0").read[Seq[FrostNoncePriv]].map(_.toVector) and
      (__ \ "pubnonces").read[Seq[ByteVector]].map(_.toVector) and
      (__ \ "aggnonces").read[Seq[ByteVector]].map(_.toVector) and
      (__ \ "msgs").read[Seq[ByteVector]].map(_.toVector) and
      (__ \ "valid_test_cases").read[Seq[SignValidTestCase]].map(_.toVector) and
      (__ \ "sign_error_test_cases")
        .read[Seq[SignErrorTestCase]]
        .map(_.toVector) and
      (__ \ "verify_fail_test_cases")
        .read[Seq[VerifyFailTestCase]]
        .map(_.toVector) and
      (__ \ "verify_error_test_cases")
        .read[Seq[VerifyErrorTestCase]]
        .map(_.toVector)
  )(SignVerifyVectors.apply _)

  // --- Tweak vectors ---
  case class TweakValidTestCase(
      id_indices: Vector[Int],
      pubshare_indices: Vector[Int],
      pubnonce_indices: Vector[Int],
      tweak_indices: Vector[Int],
      aggnonce_index: Int,
      is_xonly: Vector[Boolean],
      signer_index: Int,
      expected: ByteVector,
      comment: Option[String]
  )

  case class TweakErrorTestCase(
      id_indices: Vector[Int],
      pubshare_indices: Vector[Int],
      pubnonce_indices: Option[Vector[Int]],
      tweak_indices: Vector[Int],
      aggnonce_index: Int,
      is_xonly: Vector[Boolean],
      signer_index: Int,
      error: SignError,
      comment: Option[String]
  )

  case class TweakVectors(
      n: Int,
      t: Int,
      threshold_pubkey: ECPublicKey,
      secshare_p0: FieldElement,
      identifiers: Vector[Int],
      pubshares: Vector[ByteVector],
      secnonce_p0: FrostNoncePriv,
      pubnonces: Vector[ByteVector],
      aggnonces: Vector[ByteVector],
      tweaks: Vector[ByteVector],
      msg: ByteVector,
      valid_test_cases: Vector[TweakValidTestCase],
      error_test_cases: Vector[TweakErrorTestCase]
  )

  implicit val tweakValidReads: Reads[TweakValidTestCase] = (
    (__ \ "id_indices").read[Seq[Int]].map(_.toVector) and
      (__ \ "pubshare_indices").read[Seq[Int]].map(_.toVector) and
      (__ \ "pubnonce_indices").read[Seq[Int]].map(_.toVector) and
      (__ \ "tweak_indices").read[Seq[Int]].map(_.toVector) and
      (__ \ "aggnonce_index").read[Int] and
      (__ \ "is_xonly").read[Seq[Boolean]].map(_.toVector) and
      (__ \ "signer_index").read[Int] and
      (__ \ "expected").read[ByteVector] and
      (__ \ "comment").readNullable[String]
  )(TweakValidTestCase.apply _)

  implicit val tweakErrorReads: Reads[TweakErrorTestCase] = (
    (__ \ "id_indices").read[Seq[Int]].map(_.toVector) and
      (__ \ "pubshare_indices").read[Seq[Int]].map(_.toVector) and
      (__ \ "pubnonce_indices")
        .readNullable[Seq[Int]]
        .map(_.map(_.toVector)) and
      (__ \ "tweak_indices").read[Seq[Int]].map(_.toVector) and
      (__ \ "aggnonce_index").read[Int] and
      (__ \ "is_xonly").read[Seq[Boolean]].map(_.toVector) and
      (__ \ "signer_index").read[Int] and
      (__ \ "error").read[SignError] and
      (__ \ "comment").readNullable[String]
  )(TweakErrorTestCase.apply _)

  implicit val tweakVectorsReads: Reads[TweakVectors] = (
    (__ \ "n").read[Int] and
      (__ \ "t").read[Int] and
      (__ \ "threshold_pubkey").read[ECPublicKey] and
      (__ \ "secshare_p0").read[FieldElement] and
      (__ \ "identifiers").read[Seq[Int]].map(_.toVector) and
      (__ \ "pubshares").read[Seq[ByteVector]].map(_.toVector) and
      (__ \ "secnonce_p0").read[FrostNoncePriv] and
      (__ \ "pubnonces").read[Seq[ByteVector]].map(_.toVector) and
      (__ \ "aggnonces").read[Seq[ByteVector]].map(_.toVector) and
      (__ \ "tweaks").read[Seq[ByteVector]].map(_.toVector) and
      (__ \ "msg").read[ByteVector] and
      (__ \ "valid_test_cases")
        .read[Seq[TweakValidTestCase]]
        .map(_.toVector) and
      (__ \ "error_test_cases").read[Seq[TweakErrorTestCase]].map(_.toVector)
  )(TweakVectors.apply _)
}
