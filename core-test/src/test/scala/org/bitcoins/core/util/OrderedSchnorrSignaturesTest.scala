package org.bitcoins.core.util

import org.bitcoins.core.util.sorted.OrderedSchnorrSignatures
import org.bitcoins.crypto.{FieldElement, SchnorrDigitalSignature, SchnorrNonce}
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

class OrderedSchnorrSignaturesTest extends BitcoinSUnitTest {

  behavior of "OrderedSchnorrSignatures"

  val unsorted = Vector(
    SchnorrDigitalSignature(
      SchnorrNonce(
        "c4b89873c8753de3f0a9e94c4a6190badaa983513a6624a3469eb4577904bfea"),
      FieldElement.one),
    SchnorrDigitalSignature(
      SchnorrNonce(
        "92efe81609c773d97da2b084eb691f48ef5e926acc6eecd629f80fb1184711bc"),
      FieldElement.one)
  )

  it must "throw an exception if the signatures are out of order" in {
    intercept[IllegalArgumentException] {
      OrderedSchnorrSignatures(unsorted)
    }
  }

  it must "sort the signatures" in {
    val sorted = OrderedSchnorrSignatures.fromUnsorted(unsorted)
    assert(sorted.toVector != unsorted)
    assert(sorted.toVector == Vector(unsorted(1), unsorted(0)))
  }
}
