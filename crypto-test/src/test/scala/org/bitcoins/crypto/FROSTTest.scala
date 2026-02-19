package org.bitcoins.crypto

import org.bitcoins.crypto.frost.{
  FrostSessionContext,
  FrostShareGenResult,
  FrostUtil
}
import org.scalatest.Assertion
import scodec.bits.ByteVector

import scala.concurrent.Future
import scala.util.Random

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
    signAndVerify2of3(result, participantIds12, message)
    val participantIds13: Vector[Long] = Vector(1, 3)
    signAndVerify2of3(result, participantIds13, message)
    val participantIds23: Vector[Long] = Vector(2, 3)
    signAndVerify2of3(result, participantIds23, message)

    // reversing order shouldn't matter
    signAndVerify2of3(result, participantIds13.reverse, message)
  }

  private def signAndVerify2of3(
      result: FrostShareGenResult,
      participantIds: Vector[Long],
      message: ByteVector): Assertion = {
    val secshares =
      participantIds.map(id => result.shares.apply((id - 1).toInt))
    val secshare0 = secshares.apply(0)
    val pubshare0 = secshare0.toPoint.toPublicKey
    val secshare1 = secshares.apply(1)
    val pubshare1 = secshare1.toPoint.toPublicKey
    val participantPubShares = secshares.map(_.toPoint.toPublicKey)

    val (secnonce0, pubnonce0) = FrostUtil.nonceGen(
      rand = rand(),
      secshare = Some(secshare0),
      pubshare = Some(pubshare0),
      threshold_pk = None,
      message = Some(message),
      extra_in = None
    )
    val (secnonce1, pubnonce1) = FrostUtil.nonceGen(
      rand = rand(),
      secshare = Some(secshare1),
      pubshare = Some(pubshare1),
      threshold_pk = None,
      message = Some(message),
      extra_in = None
    )
    val secnonces = Vector(secnonce0, secnonce1)
    val pubnonces = secnonces.map(_.toNoncePub)
    val signingContext =
      result.toSigningContext(participantIds, participantPubShares)
    val aggNonce01 = FrostUtil.aggregateNonces(pubnonces = pubnonces,
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
                               signerId = participantIds(0),
                               sessionContext = sessionCtx)
    val internalVerify0 =
      FrostUtil.partialSigVerifyInternal(partialSig = pSig0,
                                         signerId = participantIds(0),
                                         pubNonce = pubnonce0,
                                         pubshare = pubshare0,
                                         sessionCtx = sessionCtx)
    assert(internalVerify0, s"internal psig0 invalid")
    val verify0 = FrostUtil.partialSigVerify(
      partialSig = pSig0,
      pubnonces = pubnonces,
      signersContext = signingContext,
      tweaks = Vector.empty,
      isXonlyT = Vector.empty,
      message = message,
      signerId = participantIds(0)
    )
    assert(verify0, s"psig0 invalid")

    val pSig1 = FrostUtil.sign(secNonce = secnonce1,
                               secShare = secshare1,
                               signerId = participantIds(1),
                               sessionContext = sessionCtx)
    val internalVerify1 =
      FrostUtil.partialSigVerifyInternal(partialSig = pSig1,
                                         signerId = participantIds(1),
                                         pubNonce = pubnonce1,
                                         pubshare = pubshare1,
                                         sessionCtx = sessionCtx)
    assert(internalVerify1, s"internal psig1 invalid")
    val verify1 = FrostUtil.partialSigVerify(
      partialSig = pSig1,
      pubnonces = pubnonces,
      signersContext = signingContext,
      tweaks = Vector.empty,
      isXonlyT = Vector.empty,
      message = message,
      signerId = participantIds(1)
    )
    assert(verify1, s"psig1 invalid")

    val sigAgg =
      FrostUtil.partialSigAgg(Vector(pSig0, pSig1), participantIds, sessionCtx)
    val verifySigAgg = signingContext.thresholdPubKey.toXOnly.schnorrPublicKey
      .verify(message, sigAgg)

    assert(verifySigAgg)
  }

}
