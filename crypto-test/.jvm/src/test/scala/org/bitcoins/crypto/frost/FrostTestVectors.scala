package org.bitcoins.crypto.frost
import org.bitcoins.crypto.BitcoinSCryptoTest
import org.bitcoins.crypto.frost.FrostJson.*
import play.api.libs.json.Json
import scodec.bits.ByteVector

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
        secshare = test.secshare
          .getOrElse(ByteVector.empty),
        pubshare = test.pubshare
          .getOrElse(ByteVector.empty),
        threshold_pk = test.threshold_pubkey
          .getOrElse(ByteVector.empty),
        message = test.msg.getOrElse(ByteVector.empty),
        extra_in = test.extra_in
      )

      assert(
        secnonce == test.expected_secnonce,
        s"Failed test: ${test.comment} expected=${test.expected_secnonce.toHex} got=${secnonce.toHex}")
      assert(
        pubnonce == test.expected_pubnonce,
        s"Failed test: ${test.comment} expected=${test.expected_pubnonce.toHex} got=${pubnonce.toHex}")

    }
  }
}
