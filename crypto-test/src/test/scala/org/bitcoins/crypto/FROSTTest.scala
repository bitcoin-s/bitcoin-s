package org.bitcoins.crypto

import org.bitcoins.crypto.frost.{FrostSessionContext, FrostUtil}
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

  private val rand = ECPrivateKey.freshPrivateKey.bytes
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
    val message =
      CryptoUtil.sha256(ByteVector.view("Hello World!".getBytes())).bytes
    val secshare0 = result.shares.apply(0)
    val pubshare0 = secshare0.toPoint.toPublicKey
    val secshare1 = result.shares.apply(1)
    val pubshare1 = secshare1.toPoint.toPublicKey

    val participantIds: Vector[Long] = Vector(1, 2)
    val participantPubShares = Vector(pubshare0, pubshare1)
    val signingContext =
      result.toSigningContext(participantIds, participantPubShares)

    val (secnonce0, pubnonce0) = FrostUtil.nonceGen(
      rand = rand,
      secshare = None,
      pubshare = None,
      threshold_pk = None,
      message = Some(message),
      extra_in = None
    )
    val (_, pubnonce1) = FrostUtil.nonceGen(
      rand = rand,
      secshare = None,
      pubshare = None,
      threshold_pk = None,
      message = Some(message),
      extra_in = None
    )
    // val secNonces = Vector(secnonce0, secnonce1)
    val pubNonces = Vector(pubnonce0, pubnonce1)

    val aggNonce01 = FrostUtil.aggregateNonces(pubnonces = pubNonces,
                                               participantIdentifiers =
                                                 participantIds)

    val sessionCtx = FrostSessionContext(
      signingContext = signingContext,
      aggNonce = aggNonce01,
      tweaks = Vector.empty,
      isXOnly = Vector.empty,
      message = message
    )
    val pSig0 = FrostUtil.sign(secNonce = secnonce0,
                               secShare = secshare0,
                               signerId = 1,
                               sessionContext = sessionCtx)
    val internalVerify0 =
      FrostUtil.partialSigVerifyInternal(partialSig = pSig0,
                                         signerId = 1,
                                         pubNonce = pubnonce0,
                                         pubshare = pubshare0,
                                         sessionCtx = sessionCtx)
    assert(internalVerify0, s"internal psig0 invalid")
    val verify0 = FrostUtil.partialSigVerify(partialSig = pSig0,
                                             pubnonces = pubNonces,
                                             signersContext = signingContext,
                                             tweaks = Vector.empty,
                                             isXonlyT = Vector.empty,
                                             message = message,
                                             signerId = 1)
    assert(verify0, s"psig0 invalid")
  }

}
