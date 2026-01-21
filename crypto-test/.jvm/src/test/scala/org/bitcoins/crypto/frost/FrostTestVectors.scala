package org.bitcoins.crypto.frost
import org.bitcoins.crypto.BitcoinSCryptoTest
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
        s"\nFailed test: ${test.comment} expected=${test.expected_secnonce.toHex} got=${secnonce.toHex}")
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
}
