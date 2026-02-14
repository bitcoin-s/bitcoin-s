package org.bitcoins.crypto

import org.bitcoins.crypto.frost.FrostUtil
import scodec.bits.ByteVector

import scala.concurrent.Future

class FROSTTest extends BitcoinSCryptoAsyncTest {
  behavior of "FROST"

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    generatorDrivenConfigNewCode

  it must "create a vss commitment and verify it" in {
    val seed1 = ByteVector.fill(31)(0x00) ++ ByteVector(0x01)
    val id1 = 1L
    val id2 = 2L
    val threshold2 = 2
    val share1 =
      FrostUtil.generateShare(seed1, threshold = threshold2, id = id1)
    val share2 =
      FrostUtil.generateShare(seed1, threshold = threshold2, id = id2)
    val commitment2 = FrostUtil.vssCommitment(seed1, threshold = threshold2)

    assert(FrostUtil.vssVerify(share1, id = id1, commitments = commitment2))
    assert(FrostUtil.vssVerify(share2, id = id2, commitments = commitment2))
  }

  it must "create/verify vss commitments for arbitrary thresholds" in {
    forAllAsync(NumberGenerator.bytevector(32),
                // threshold must be > 1 , max = 127 to run tests in a reasonable time
                NumberGenerator.positiveByte.suchThat(_ > 0)) {
      case (seed, threshold) =>
        Future {
          val shares = 1.until(threshold + 1).map { id =>
            FrostUtil.generateShare(seed, threshold = threshold, id = id)
          }
          val commitments = FrostUtil.vssCommitment(seed, threshold = threshold)
          shares.zipWithIndex.foreach { case (share, idx) =>
            val id = idx + 1L
            assert(
              FrostUtil.vssVerify(share, id = id, commitments = commitments))
          }
          succeed
        }
    }
  }

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
      assert(FrostUtil.vssVerify(share, id, result.commitments))
    }
    succeed
  }

}
