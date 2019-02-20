package org.bitcoins.core.protocol.ln

import org.bitcoins.core.crypto.ECPrivateKey
import org.bitcoins.testkit.core.gen.CryptoGenerators
import org.bitcoins.testkit.core.gen.ln.LnInvoiceGen
import org.bitcoins.core.util.BitcoinSUnitTest

class LnInvoiceSignatureTest extends BitcoinSUnitTest {

  override implicit val generatorDrivenConfig: PropertyCheckConfiguration =
    generatorDrivenConfigNewCode

  behavior of "LnInvoiceSignature"

  it must "have serialization symmetry for raw r,s,recovId" in {
    forAll(CryptoGenerators.digitalSignature, LnInvoiceGen.signatureVersion) {
      case (ecSig, recovId) =>
        val lnSig = LnInvoiceSignature.fromRS(r = ecSig.r.bigInteger,
                                              s = ecSig.s.bigInteger,
                                              recovId = recovId)

        val serialized = lnSig.hex

        val deserialized = LnInvoiceSignature.fromHex(serialized)

        assert(deserialized.signature.r == ecSig.r)
        assert(deserialized.signature.s == ecSig.s)
        assert(deserialized.recoverId == recovId)

    }
  }

  it must "have serialization symmetry" in {
    forAll(LnInvoiceGen.lnInvoiceSignature) {
      case sig =>
        assert(LnInvoiceSignature.fromHex(sig.hex) == sig)
    }
  }

  it must "be able to generate signatures, and then verify those signatures" in {
    val gen = LnInvoiceGen
    forAll(gen.lnHrp, gen.taggedFields(None), gen.invoiceTimestamp) {
      case (hrp, tags, timestamp) =>
        val key = ECPrivateKey.freshPrivateKey
        val signature = LnInvoice.buildLnInvoiceSignature(
          hrp = hrp,
          timestamp = timestamp,
          lnTags = tags,
          privateKey = key
        )

        val hash = LnInvoice.buildSigHashData(
          hrp = hrp,
          timestamp = timestamp,
          lnTags = tags
        )

        assert(key.publicKey.verify(hash, signature.signature))
    }
  }
}
