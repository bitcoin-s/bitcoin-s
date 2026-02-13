package org.bitcoins.crypto

import org.bitcoins.crypto.frost.FrostUtil

class FROSTTest extends BitcoinSCryptoTest {
  behavior of "FROST"

  it must "create a simple 2/3 threshold and be example to produce valid signatures for all combinations of the threshold" in {
    val seed = ECPrivateKey.freshPrivateKey
    val threshold = 2
    val numShares = 3
    val result = FrostUtil.generateShares(seed,
                                          threshold = threshold,
                                          numShares = numShares)

    assert(result.ids.size == numShares)
    assert(result.commitments.size == threshold)
    assert(result.shares.size == numShares)

    result.shares.zip(result.ids).foreach { case (share, id) =>
      println(s"Verifying $id share=$share")
      assert(FrostUtil.vssVerify(share, id, result.commitments))
    }
  }

}
