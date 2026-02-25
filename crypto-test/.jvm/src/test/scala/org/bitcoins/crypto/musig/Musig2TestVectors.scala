package org.bitcoins.crypto.musig

import org.bitcoins.commons.serializers.JsonReaders.ECPublicKeyReads
import org.bitcoins.crypto.*
import play.api.libs.json.*

import scala.io.Source
import scala.util.Using

class Musig2TestVectors extends BitcoinSCryptoTest {
  behavior of "Musig2"

  it must "pass key_sort_vectors.json" in {

    val fileName = "/musig2/key_sort_vectors.json"
    val lines = Using(Source.fromURL(getClass.getResource(fileName))) {
      source => source.mkString
    }.get
    val json = Json.parse(lines)

    val pubkeys: Vector[ECPublicKey] =
      (json \ "pubkeys").validate[Vector[ECPublicKey]].get

    val sortedPubkeys: Vector[ECPublicKey] =
      (json \ "sorted_pubkeys").validate[Vector[ECPublicKey]].get

    // Build UnsortedKeySet from parsed pubkeys and assert the sorted keys equal expected
    val keySet = UnsortedKeySet(pubkeys)
    val actualSorted = keySet.toSorted

    assert(actualSorted.keys == sortedPubkeys)
  }

  it must "pass key_agg_vectors.json" in {

    val fileName = "/musig2/key_agg_vectors.json"
    val lines = Using(Source.fromURL(getClass.getResource(fileName))) {
      source => source.mkString
    }.get
    val json = Json.parse(lines)

    val parsed = json.validate[Musig2Json.KeyAggVectors]
    parsed.fold(
      errs => fail(s"Failed to parse key_agg_vectors.json: $errs"),
      vecs => {
        // Check the valid test cases
        vecs.valid_test_cases.foreach { tc =>
          val keys = tc.key_indices.map(idx => vecs.pubkeys(idx)).toVector
          val keySet = UnsortedKeySet(keys.map(_.toPublicKey))
          assert(keySet.aggPubKey.schnorrPublicKey == tc.expected)
        }

        // For error test cases we just ensure appropriate failures are raised
        vecs.error_test_cases.foreach { etc =>
          val keys = etc.key_indices.map(idx => vecs.pubkeys(idx)).toVector
          val tweaks = etc.tweak_indices.map(i => vecs.tweaks(i)).toVector
          // Build KeySet and expect either construction or aggPubKey to throw
          intercept[Exception] {
            val tweaksWXonly = tweaks.zip(etc.is_xonly)
            val musigTweaks = tweaksWXonly.map(t =>
              MuSigTweak(FieldElement.fromBytes(t._1), isXOnlyT = t._2))
            val kset =
              UnsortedKeySet(keys.map(_.toPublicKey))
                .withTweaks(musigTweaks)
            // Force aggPubKey computation
            kset.aggPubKey
          }
        }
      }
    )
  }

  it must "pass nonce_gen_vectors.json" in {
    val fileName = "/musig2/nonce_gen_vectors.json"
    val lines = Using(Source.fromURL(getClass.getResource(fileName))) {
      source => source.mkString
    }.get
    val json = Json.parse(lines)

    val vecs = json.validate[Musig2Json.NonceGenVectors].get

    vecs.test_cases.foreach { test =>
      val preRand = test.rand_
      val noncePriv =
        MuSigUtil.nonceGen(
          preRand = preRand,
          publicKey = test.pk,
          privKeyOpt = test.sk.map(s => ECPrivateKey.fromBytes(s.bytes)),
          aggPubKeyOpt = test.aggpk,
          msgOpt = test.msg,
          extraInOpt = test.extra_in
        )
      val pubnonce = noncePriv.toNoncePub

      assert(
        noncePriv == test.expected_secnonce,
        s"expected=${test.expected_secnonce.toStringSensitive} actual=${noncePriv.toStringSensitive}")
      assert(pubnonce == test.expected_pubnonce)
    }
  }

}
