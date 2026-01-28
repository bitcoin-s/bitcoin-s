package org.bitcoins.crypto.frost
import org.bitcoins.crypto.{BitcoinSCryptoTest, ECPublicKey, FieldElement}
import org.bitcoins.crypto.frost.FrostJson.*
import play.api.libs.json.Json

import scala.io.Source
import scala.util.Using

class FrostTestVectors extends BitcoinSCryptoTest {
  behavior of "FrostTestVectors"

  it should "pass nonce_gen_vectors.json" in {
    val fileName = "/nonce_gen_vectors.json"
    val lines = Using(Source.fromURL(getClass.getResource(fileName))) {
      source => source.mkString
    }.get
    val json = Json.parse(lines)
    val testCases = json.validate[NonceGenTestVectors].get

    testCases.test_cases.foreach { test =>
      val (secnonce, pubnonce) = FrostUtil.nonceGen(
        rand = test.rand,
        secshare = test.secshare,
        pubshare = test.pubshare,
        threshold_pk = test.threshold_pubkey,
        message = test.msg,
        extra_in = test.extra_in
      )

      assert(
        secnonce == test.expected_secnonce,
        s"\nFailed test: ${test.comment} expected=${test.expected_secnonce.hex} got=${secnonce.hex}")
      assert(
        pubnonce == test.expected_pubnonce,
        s"\nFailed test: ${test.comment} expected=${test.expected_pubnonce.hex} got=${pubnonce.hex}")

    }
  }

  it must "pass nonce_agg_vectors.json" in {
    val fileName = "/nonce_agg_vectors.json"
    val lines = Using(Source.fromURL(getClass.getResource(fileName))) {
      source => source.mkString
    }.get
    val json = Json.parse(lines)
    val testCases = json.validate[NonceAggTestVectors].get

    // Valid test cases
    testCases.valid_test_cases.foreach { test =>
      val pubnoncesToAgg = test.pubnonce_indices
        .map { idx =>
          testCases.pubnonces(idx)
        }
        .map(FrostNoncePub.fromBytes)
      val aggNonce = FrostUtil.aggregateNonces(
        pubnonces = pubnoncesToAgg,
        participantIdentifiers = test.participant_identifiers
      )
      assert(
        aggNonce == test.expected_aggnonce,
        s"\nFailed test: ${test.comment.getOrElse(
            "")} expected=${test.expected_aggnonce.hex} got=${aggNonce.hex}"
      )
    }

    // Error test cases
    testCases.error_test_cases.foreach { test =>
      val pubnoncesToAgg = test.pubnonce_indices.map { idx =>
        testCases.pubnonces(idx)
      }

      assertThrows[Exception] {
        FrostUtil.aggregateNonces(
          pubnonces = pubnoncesToAgg.map(FrostNoncePub.fromBytes),
          participantIdentifiers = test.participant_identifiers
        )
      }
    }
  }

  it should "pass tweak_vectors.json" in {
    val fileName = "/tweak_vectors.json"
    val lines = Using(Source.fromURL(getClass.getResource(fileName))) {
      source =>
        source.mkString
    }.get

    val json = Json.parse(lines)
    val vecs = json.validate[TweakVectors].get

    // Basic assertions to ensure parsing succeeded
    vecs.valid_test_cases.foreach { t =>
      val participantIds = t.id_indices.map(vecs.identifiers(_))
      val pubshares = t.pubshare_indices.map(vecs.pubshares(_))
      val pubnonces = t.pubnonce_indices.map(vecs.pubnonces(_))
      val tweaks =
        t.tweak_indices.map(vecs.tweaks(_)).map(FieldElement.fromBytes)
      val signingContext = FrostSigningContext(
        n = vecs.n,
        t = vecs.t,
        ids = participantIds,
        pubshares = pubshares.map(ECPublicKey.fromBytes),
        thresholdPubKey = vecs.threshold_pubkey
      )
      val sessionCtx = FrostSessionContext(
        signingContext = signingContext,
        aggNonce = FrostUtil.aggregateNonces(
          pubnonces = pubnonces.map(FrostNoncePub.fromBytes),
          participantIdentifiers = participantIds
        ),
        tweaks = tweaks,
        isXOnly = t.is_xonly,
        message = vecs.msg
      )
      val result = FrostUtil.sign(secNonce = vecs.secnonce_p0,
                                  secShare = vecs.secshare_p0,
                                  myId = t.id_indices(t.signer_index),
                                  sessionContext = sessionCtx)
      assert(
        result.bytes == t.expected,
        s"\nFailed test: ${t.comment.getOrElse("")} expected=${t.expected} got=${result.hex}")
    }

    // Error test cases: ensure expected errors are raised
    vecs.error_test_cases.foreach { err =>
      val participantIds = err.id_indices.map(vecs.identifiers(_))
      val pubshares = err.pubshare_indices.map(vecs.pubshares(_))
      assertThrows[IllegalArgumentException] {
        val pubnoncesOpt = err.pubnonce_indices.map(
          _.map(vecs.pubnonces(_)).map(FrostNoncePub.fromBytes))
        val tweaks =
          err.tweak_indices.map(vecs.tweaks(_)).map(FieldElement.fromBytes)
        val signingContext = FrostSigningContext(
          n = vecs.n,
          t = vecs.t,
          ids = participantIds,
          pubshares = pubshares.map(ECPublicKey.fromBytes),
          thresholdPubKey = vecs.threshold_pubkey
        )
        val aggNonce = FrostUtil.aggregateNonces(
          pubnonces = pubnoncesOpt.getOrElse(Vector.empty),
          participantIdentifiers = participantIds
        )
        val sessionCtx = FrostSessionContext(
          signingContext = signingContext,
          aggNonce = aggNonce,
          tweaks = tweaks,
          isXOnly = err.is_xonly,
          message = vecs.msg
        )
        // Attempt to sign; any of the above steps may throw for malformed inputs
        FrostUtil.sign(secNonce = vecs.secnonce_p0,
                       secShare = vecs.secshare_p0,
                       myId = err.id_indices(err.signer_index),
                       sessionContext = sessionCtx)
      }
    }
    succeed
  }

  it must "pass sign_verify_vectors.json" in {
    val fileName = "/sign_verify_vectors.json"
    val lines = Using(Source.fromURL(getClass.getResource(fileName))) {
      source =>
        source.mkString
    }.get

    val json = Json.parse(lines)
    val vecs = json.validate[SignVerifyVectors].get

    // Valid sign test cases
    vecs.valid_test_cases.foreach { t =>
      val participantIds = t.id_indices.map(vecs.identifiers(_).toLong)
      val pubshares =
        t.pubshare_indices.map(vecs.pubshares(_)).map(ECPublicKey.fromBytes)
      val pubnonces =
        t.pubnonce_indices.map(vecs.pubnonces(_)).map(FrostNoncePub.fromBytes)
      val aggNonce = FrostUtil.aggregateNonces(pubnonces, participantIds)
      val msg = vecs.msgs(t.msg_index)

      val signingContext = FrostSigningContext(
        n = vecs.n,
        t = vecs.t,
        ids = participantIds,
        pubshares = pubshares,
        thresholdPubKey = vecs.threshold_pubkey
      )

      // Use the first secnonce for p0 as the secret nonces to sign with
      val secNonce = vecs.secnonces_p0.head
      val secShare = FieldElement.fromBytes(vecs.secshare_p0)
      val sessionCtx = FrostSessionContext(
        signingContext = signingContext,
        aggNonce = aggNonce,
        tweaks = Vector.empty,
        isXOnly = Vector.empty,
        message = msg
      )

      val s = FrostUtil.sign(
        secNonce = secNonce,
        secShare = secShare,
        myId = t.id_indices(t.signer_index).toLong,
        sessionContext = sessionCtx
      )

      assert(
        s.bytes == t.expected,
        s"\nFailed test: ${t.comment.getOrElse("")} expected=${t.expected} got=${s.hex}")
    }

    // Sign error test cases: ensure expected errors are raised
//    vecs.sign_error_test_cases.foreach { err =>
//      assertThrows[IllegalArgumentException] {
//        val participantIds = err.id_indices.map(vecs.identifiers(_).toLong)
//        val pubshares = err.pubshare_indices.map(vecs.pubshares(_)).map(ECPublicKey.fromBytes)
//        val aggNonce = FrostNoncePub.fromBytes(vecs.aggnonces(err.aggnonce_index))
//        val secnonceOpt = err.secnonce_index.flatMap(idx => vecs.secnonces_p0.lift(idx))
//        val signingContext = FrostSigningContext(
//          n = vecs.n,
//          t = vecs.t,
//          ids = participantIds,
//          pubshares = pubshares,
//          thresholdPubKey = vecs.threshold_pubkey
//        )
//        val sessionCtx = FrostSessionContext(
//          signingContext = signingContext,
//          aggNonce = aggNonce,
//          tweaks = Vector.empty,
//          isXOnly = Vector.empty,
//          message = vecs.msgs(err.msg_index)
//        )
//        // Attempt to sign (may throw earlier during construction)
//        FrostUtil.sign(
//          secNonce = secnonceOpt.getOrElse(vecs.secnonces_p0.head),
//          secShare = FieldElement.fromBytes(vecs.secshare_p0),
//          myId = err.signer_id.map(_.toLong).getOrElse(vecs.identifiers(err.signer_index).toLong),
//          sessionContext = sessionCtx
//        )
//      }
//    }
//
//    // Verify-fail test cases: partialSigVerify should return false
//    vecs.verify_fail_test_cases.foreach { vtc =>
//      val participantIds = vtc.id_indices.map(vecs.identifiers(_).toLong)
//      val pubshares = vtc.pubshare_indices.map(vecs.pubshares(_)).map(ECPublicKey.fromBytes)
//      val pubnonces = vtc.pubnonce_indices.map(vecs.pubnonces(_)).map(FrostNoncePub.fromBytes)
//      val msg = vecs.msgs(vtc.msg_index)
//      val psig = FieldElement.fromBytes(vtc.psig)
//
//      val signingContext = FrostSigningContext(
//        n = vecs.n,
//        t = vecs.t,
//        ids = participantIds,
//        pubshares = pubshares,
//        thresholdPubKey = vecs.threshold_pubkey
//      )
//
//      val result = FrostUtil.partialSigVerify(
//        partialSig = psig,
//        pubnonces = pubnonces,
//        signersContext = signingContext,
//        tweaks = Vector.empty,
//        isXonlyT = Vector.empty,
//        message = msg,
//        i = participantIds.head
//      )
//
//      assert(!result, s"Expected verification to fail for test: ${vtc.comment}")
//    }
//
//    // Verify error test cases: ensure expected errors are raised
//    vecs.verify_error_test_cases.foreach { etc =>
//      assertThrows[IllegalArgumentException] {
//        val participantIds = etc.id_indices.map(vecs.identifiers(_).toLong)
//        val pubshares = etc.pubshare_indices.map(vecs.pubshares(_)).map(ECPublicKey.fromBytes)
//        val pubnonces = etc.pubnonce_indices.map(vecs.pubnonces(_)).map(FrostNoncePub.fromBytes)
//        val msg = vecs.msgs(etc.msg_index)
//        val psig = FieldElement.fromBytes(etc.psig)
//
//        val signingContext = FrostSigningContext(
//          n = vecs.n,
//          t = vecs.t,
//          ids = participantIds,
//          pubshares = pubshares,
//          thresholdPubKey = vecs.threshold_pubkey
//        )
//
//        // Attempt to verify - underlying functions should throw for malformed inputs
//        FrostUtil.partialSigVerify(
//          partialSig = psig,
//          pubnonces = pubnonces,
//          signersContext = signingContext,
//          tweaks = Vector.empty,
//          isXonlyT = Vector.empty,
//          message = msg,
//          i = participantIds.head
//        )
//      }
//    }

    succeed
  }
}
