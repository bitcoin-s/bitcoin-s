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

    val vectors = lines.map(PresigVector.fromCsvLine).toVector

    vectors.foreach { t =>
      if (
        t.secretKey.isDefined && t.publicKey.isSuccess && t.preSignature.isSuccess && t.adaptor.isSuccess
      ) {
        val presig = AdaptorUtil.schnorrAdaptorSign(t.secretKey.get,
                                                    t.adaptor.get,
                                                    t.message.get,
                                                    t.auxRand)
        assert(presig == t.preSignature.get)
      } else if (
        t.publicKey.isSuccess && t.preSignature.isSuccess && t.adaptor.isSuccess
      ) {
        try {
          val result = AdaptorUtil.schnorrAdaptorVerify(
            t.preSignature.get,
            t.publicKey.get,
            t.message.getOrElse(ByteVector.empty),
            t.adaptor.get)
          assert(result == t.result)
        } catch {
          case scala.util.control.NonFatal(_) =>
            // make sure we expected test case to fail
            // if an exception is thrown
            assert(!t.result)
        }

      } else {
        // means we failed to parse the test vector, make sure we expected that to happen
        assert(!t.result)
      }
    }
  }
}
