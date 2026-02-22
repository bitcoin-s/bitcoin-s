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
          val keySet = UnsortedKeySet(keys)
          assert(keySet.aggPubKey.schnorrPublicKey == tc.expected)
        }

        // For error test cases we just ensure appropriate failures are raised
        vecs.error_test_cases.foreach { etc =>
          val keys = etc.key_indices.map(idx => vecs.pubkeys(idx)).toVector
          val tweaks = etc.tweak_indices.map(i => vecs.tweaks(i)).toVector
          // Build KeySet and expect either construction or aggPubKey to throw
          intercept[Exception] {
            val kset = UnsortedKeySet(keys).withTweaks(tweaks.map(t =>
              MuSigTweak(t, isXOnlyT = true)))
            // Force aggPubKey computation
            kset.aggPubKey
          }
        }
      }
    )
  }

}
