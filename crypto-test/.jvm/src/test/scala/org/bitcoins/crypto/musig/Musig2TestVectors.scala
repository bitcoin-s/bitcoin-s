package org.bitcoins.crypto.musig

import org.bitcoins.commons.serializers.JsonReaders.ECPublicKeyReads
import org.bitcoins.crypto.*
import play.api.libs.json.*

import scala.io.Source

class Musig2TestVectors extends BitcoinSCryptoTest {
  behavior of "Musig2"

  it must "pass key_sort_vectors.json" in {
    val stream = getClass.getClassLoader.getResourceAsStream(
      "musig2/key_sort_vectors.json")
    require(stream != null,
            "Could not find musig2/key_sort_vectors.json in resources")
    val rawText = Source.fromInputStream(stream).getLines().mkString("\n")
    stream.close()

    val json =
      try Json.parse(rawText)
      catch {
        case e: Throwable =>
          fail(
            s"Failed to parse JSON musig2/key_sort_vectors.json: ${e.getMessage}\nContent:\n$rawText")
      }

    val pubkeys: Vector[ECPublicKey] =
      (json \ "pubkeys").validate[Vector[ECPublicKey]].get

    val sortedPubkeys: Vector[ECPublicKey] =
      (json \ "sorted_pubkeys").validate[Vector[ECPublicKey]].get

    // Build UnsortedKeySet from parsed pubkeys and assert the sorted keys equal expected
    val keySet = UnsortedKeySet(pubkeys)
    val actualSorted = keySet.toSorted

    assert(actualSorted.keys == sortedPubkeys)
  }
}
