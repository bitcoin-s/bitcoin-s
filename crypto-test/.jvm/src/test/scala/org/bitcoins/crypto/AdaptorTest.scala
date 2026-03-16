package org.bitcoins.crypto

import scodec.bits.ByteVector

import scala.io.Source

class AdaptorTest extends BitcoinSCryptoTest {
  behavior of "AdaptorUtil"

  it must "pass presig_vectors.csv" in {
    val stream =
      getClass.getResourceAsStream("/adaptor-sigs/schnorr/presig_vectors.csv")
    val source = Source.fromInputStream(stream)
    val lines = source.getLines().drop(2) // Skip header and comment line

    val vectors = lines.take(3).map(PresigVector.fromCsvLine).toVector

    vectors.foreach { t =>
      if (t.secretKey.isDefined) {
        val presig = AdaptorUtil.schnorrAdaptorSign(t.secretKey.get,
                                                    t.adaptor,
                                                    t.message.get,
                                                    t.auxRand)
        assert(presig == t.preSignature)
      } else {
        val result = AdaptorUtil.schnorrAdaptorVerify(
          t.preSignature,
          t.publicKey,
          t.message.getOrElse(ByteVector.empty),
          t.adaptor)
        assert(result == t.result)
      }

    }
  }
}
