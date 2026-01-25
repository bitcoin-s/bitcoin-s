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
      val pubnoncesToAgg = test.pubnonce_indices.map { idx =>
        testCases.pubnonces(idx)
      }
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

      assertThrows[IllegalArgumentException] {
        FrostUtil.aggregateNonces(
          pubnonces = pubnoncesToAgg,
          participantIdentifiers = test.participant_identifiers
        )
      }
    }
  }

  it should "pass sign_verify_vectors.json" in {
    val fileName = "/sign_verify_vectors.json"
    val lines = Using(Source.fromURL(getClass.getResource(fileName))) {
      source =>
        source.mkString
    }.get

    val json = Json.parse(lines)
    val vecs = json.validate[SignVerifyVectors].get

    vecs.valid_test_cases.foreach { t =>
      // val msg = vecs.msgs(t.msg_index)
      val pubshareBytes = t.pubshare_indices.map(vecs.pubshares)
      val pubshares = pubshareBytes.map(ECPublicKey.fromBytes)
      // val pubnonceBytes = t.pubnonce_indices.map(vecs.pubnonces)
      // val pubnonces = pubnonceBytes.map(ECPublicKey.fromBytes)
      // val aggnonce = FrostNonce.fromBytes(vecs.aggnonces(t.aggnonce_index))
      val _ = FrostSigningContext(
        n = vecs.n,
        t = vecs.t,
        ids = t.id_indices.map(vecs.identifiers(_)),
        // drop last share for now as it isn't a valid point on the curve
        pubshares = pubshares,
        thresholdPubKey = vecs.threshold_pubkey
      )
    }
  }

  it should "parse tweak_vectors.json" in {
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
          pubnonces = pubnonces,
          participantIdentifiers = participantIds
        ),
        tweaks = tweaks,
        isXOnly = t.is_xonly,
        message = vecs.msg
      )
      val result = FrostUtil.sign(secNonce = vecs.secnonce_p0,
                                  secShare = vecs.secshare_p0,
                                  myId = vecs.identifiers(t.signer_index),
                                  sessionContext = sessionCtx)
      assert(
        result.bytes == t.expected,
        s"\nFailed test: ${t.comment.getOrElse("")} expected=${t.expected} got=${result.hex}")
    }
    succeed
  }
}
