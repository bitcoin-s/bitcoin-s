package org.bitcoins.crypto

import org.bitcoins.crypto.frost.*
import org.scalatest.Assertion
import scodec.bits.ByteVector

import scala.concurrent.Future
import scala.util.Random

class FROSTTest extends BitcoinSCryptoAsyncTest {
  behavior of "FROST"

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

  private def rand(): ByteVector = ByteVector.view(Random.nextBytes(32))
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

    val participantIds12: Vector[Long] = Vector(1, 2)
    signAndVerify(result, participantIds12, message, tweaks = Vector.empty)
    val participantIds13: Vector[Long] = Vector(1, 3)
    signAndVerify(result, participantIds13, message, tweaks = Vector.empty)
    val participantIds23: Vector[Long] = Vector(2, 3)
    signAndVerify(result, participantIds23, message, tweaks = Vector.empty)

    // reversing order shouldn't matter
    signAndVerify(result,
                  participantIds13.reverse,
                  message,
                  tweaks = Vector.empty)

    // tweaks should work as well
    signAndVerify(result,
                  participantIds12,
                  message,
                  tweaks = Vector((FieldElement.one, Random.nextBoolean())))
  }

  it must "sign and verify an arbitrary t/n with arbitrary tweaks" in {
    forAllAsync(CryptoGenerators.privateKey,
                NumberGenerator.positiveByte.suchThat(_ > 1),
                CryptoGenerators.sha256Digest,
                CryptoGenerators.tweaks) {
      case (seed, numShares, message, tweaks) =>
        Future {
          val threshold = getThreshold(numShares)

          val result = FrostUtil.generateShares(seed,
                                                threshold = threshold,
                                                numShares = numShares)
          val participantIds = result.ids.take(threshold.toInt)
          signAndVerify(result, participantIds, message.bytes, tweaks)
        }
    }
  }

  private def signAndVerify(
      result: FrostShareGenResult,
      participantIds: Vector[Long],
      message: ByteVector,
      tweaks: Vector[(FieldElement, Boolean)]): Assertion = {
    val secshares =
      participantIds.map(id => result.shares.apply((id - 1).toInt))
    val participantPubShares = secshares.map(_.toPoint.toPublicKey)

    val secAndPubNonces: Vector[(FieldElement, FrostNoncePriv, FrostNoncePub)] =
      secshares.map { secShare =>
        val x = FrostUtil.nonceGen(
          rand = rand(),
          secshare = Some(secShare),
          pubshare = Some(secShare.toPoint.toPublicKey),
          threshold_pk = None,
          message = Some(message),
          extra_in = None
        )
        (secShare, x._1, x._2)
      }

    // val secnonces = secAndPubNonces.map(_._2)
    val pubnonces = secAndPubNonces.map(_._3)
    val signingContext =
      result.toSigningContext(participantIds, participantPubShares)
    val aggNonce = FrostUtil.aggregateNonces(pubnonces = pubnonces,
                                             participantIdentifiers =
                                               participantIds)

    val sessionCtx = FrostSessionContext(
      signingContext = signingContext,
      aggNonce = aggNonce,
      tweaks = tweaks.map(_._1),
      isXOnly = tweaks.map(_._2),
      message = message
    )
    val pSigs = secAndPubNonces.zipWithIndex.map {
      case ((secshare, secnonce, pubnonce), idx) =>
        val signerId = participantIds(idx)
        val pubshare = secshare.toPoint.toPublicKey
        val pSig = FrostUtil.sign(secNonce = secnonce,
                                  secShare = secshare,
                                  signerId = signerId,
                                  sessionContext = sessionCtx)
        val internalVerify0 =
          FrostUtil.partialSigVerifyInternal(partialSig = pSig,
                                             signerId = signerId,
                                             pubNonce = pubnonce,
                                             pubshare = pubshare,
                                             sessionCtx = sessionCtx)
        assert(internalVerify0,
               s"internal psig0 invalid for signerId=$signerId")
        val verify0 = FrostUtil.partialSigVerify(
          partialSig = pSig,
          pubnonces = pubnonces,
          signersContext = signingContext,
          tweaks = tweaks.map(_._1),
          isXonlyT = tweaks.map(_._2),
          message = message,
          signerId = signerId
        )
        assert(verify0, s"psig invalid for signerId=$signerId")
        pSig
    }
    val sigAgg =
      FrostUtil.partialSigAgg(pSigs, participantIds, sessionCtx)
    val verifySigAgg = {
      if (tweaks.isEmpty) {
        signingContext.thresholdPubKey.toXOnly
          .verify(message, sigAgg)
      } else {
        val tweakedKey = FrostTweakContext.calculateTweakedKey(
          signingContext.thresholdPubKey,
          tweaks.map(_._1),
          tweaks.map(_._2)
        )
        tweakedKey.toXOnly.verify(message, sigAgg)
      }

    }

    assert(
      verifySigAgg,
      s"Aggregated signature for participantIds $participantIds failed to verify")
  }

  private def getThreshold(numShares: Byte): Int = {
    if (numShares == 2) {
      // for the case where numShares is 2
      // we want to make sure we test the threshold of 2/2
      2
    } else {
      Random.between(2, numShares)
    }
  }
}
