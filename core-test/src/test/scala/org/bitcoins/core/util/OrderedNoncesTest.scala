package org.bitcoins.core.util

import org.bitcoins.core.util.sorted.OrderedNonces
import org.bitcoins.crypto.SchnorrNonce
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

class OrderedNoncesTest extends BitcoinSUnitTest {

  behavior of "OrderedNonces"

  val unsorted = Vector(
    SchnorrNonce(
      "c4b89873c8753de3f0a9e94c4a6190badaa983513a6624a3469eb4577904bfea"),
    SchnorrNonce(
      "92efe81609c773d97da2b084eb691f48ef5e926acc6eecd629f80fb1184711bc")
  )

  it must "throw an exception if you create an unordered nonces" in {

    intercept[IllegalArgumentException] {
      OrderedNonces(unsorted)
    }
  }

  it must "sort nonces with OrderedNonces.fromUnsorted" in {
    val sorted = OrderedNonces.fromUnsorted(unsorted)
    assert(sorted.toVector != unsorted)
    assert(sorted.toVector == Vector(unsorted(1), unsorted(0)))
  }
}
