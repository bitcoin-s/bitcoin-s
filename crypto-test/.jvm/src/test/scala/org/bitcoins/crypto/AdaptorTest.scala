package org.bitcoins.crypto

import scodec.bits.ByteVector

import scala.io.Source
import scala.util.Using

class AdaptorTest extends BitcoinSCryptoTest {
  behavior of "AdaptorUtil"

  it must "pass presig_vectors.csv" in {
    val stream =
      getClass.getResourceAsStream("/adaptor-sigs/schnorr/presig_vectors.csv")
    val vectors = Using.resource(Source.fromInputStream(stream)) { source =>
      val lines = source.getLines().drop(2) // Skip header and comment line
      lines.map(PresigVector.fromCsvLine).toVector
    }

    vectors.foreach { t =>
      if (
        t.secretKey.isDefined && t.publicKey.isSuccess && t.preSignature.isSuccess && t.adaptor.isSuccess
      ) {
        val presig = AdaptorUtil.schnorrAdaptorSign(t.secretKey.get,
                                                    t.adaptor.get,
                                                    t.message.get,
                                                    t.auxRand.get)
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

  it must "pass adapt_vectors.csv" in {
    val stream =
      getClass.getResourceAsStream("/adaptor-sigs/schnorr/adapt_vectors.csv")
    val vectors = Using.resource(Source.fromInputStream(stream)) { source =>
      val lines = source.getLines().drop(2) // Skip header and comment line
      lines.map(AdaptVector.fromCsvLine).toVector
    }

    vectors.foreach { t =>
      if (
        t.publicKey.isSuccess && t.adaptorSecret.isSuccess && t.preSignature.isSuccess && t.signature.isSuccess
      ) {
        val adaptedSig = AdaptorUtil.schnorrAdaptorComplete(t.adaptorSecret.get,
                                                            t.preSignature.get)
        assert(adaptedSig == t.signature.get,
               s"Signature mismatch: ${t.comment}")
        val verifies = t.publicKey.get.verify(t.message, adaptedSig)
        assert(verifies == t.result, s"Verification mismatch: ${t.comment}")
      } else {
        assert(!t.result, s"Parsing failed for valid vector: ${t.comment}")
      }
    }
  }

  it must "pass secadaptor_vectors.csv" in {
    val stream =
      getClass.getResourceAsStream(
        "/adaptor-sigs/schnorr/secadaptor_vectors.csv")
    val vectors = Using.resource(Source.fromInputStream(stream)) { source =>
      val lines = source.getLines().drop(2) // Skip header and comment line
      lines.map(SecAdaptorVector.fromCsvLine).toVector
    }

    vectors.foreach { t =>
      if (
        t.preSignature.isSuccess && t.signature.isSuccess && t.adaptorSecret.isSuccess
      ) {
        val adaptorPub = t.adaptorSecret.get.publicKey

        try {
          val extracted = AdaptorUtil.schnorrExtractSecret(t.signature.get,
                                                           t.preSignature.get,
                                                           adaptorPub)
          assert(t.result, s"Expected failure but succeeded for ${t.comment}")
          assert(extracted == t.adaptorSecret.get)
        } catch {
          case scala.util.control.NonFatal(e) =>
            if (t.result) fail(s"Failed to extract secret: ${e.getMessage}")
            else {
              assert(!t.result)
            }
        }
      } else {
        // If parsing failed, and result is TRUE, that is fail.
        assert(!t.result, s"Parsing failed for valid vector: ${t.comment}")
      }
    }
  }
}
