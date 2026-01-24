package org.bitcoins.crypto.frost

import org.bitcoins.crypto.{
  CryptoParams,
  CryptoUtil,
  ECPublicKey,
  FieldElement,
  SecpPoint,
  SecpPointInfinity,
  XOnlyPubKey
}
import scodec.bits.ByteVector

object FrostUtil {

  def hashFrostAux(bytes: ByteVector): ByteVector = {
    CryptoUtil.taggedSha256(bytes, "FROST/aux").bytes
  }

  def hashFrostNonce(bytes: ByteVector): ByteVector = {
    CryptoUtil.taggedSha256(bytes, "FROST/nonce").bytes
  }

  def nonceGen(
      rand: ByteVector,
      secshare: Option[ByteVector],
      pubshare: Option[ECPublicKey],
      threshold_pk: Option[XOnlyPubKey],
      message: Option[ByteVector],
      extra_in: Option[ByteVector]): (FrostNoncePriv, FrostNoncePub) = {
    val randPrime = secshare match {
      case Some(sec) => sec.xor(hashFrostAux(rand))
      case None      => rand
    }

    // Match the Python reference: None -> 0x00, Some(msg) -> 0x01 || len(msg,8) || msg
    // Note: an explicit empty message (Some(ByteVector.empty)) must be encoded as
    // 0x01 followed by 8 zero bytes (length 0), which differs from None.
    val mPrefix = message match {
      case Some(m) =>
        ByteVector.fromByte(1) ++ ByteVector.fromLong(m.length, 8) ++ m
      case None => ByteVector.fromByte(0)
    }

    val preimages: Vector[FieldElement] = 0
      .until(2)
      .map { i =>
        val b = randPrime ++
          ByteVector.fromLong(pubshare.map(_.bytes.size).getOrElse(0), 1) ++
          pubshare.map(_.bytes).getOrElse(ByteVector.empty) ++
          ByteVector.fromLong(threshold_pk.map(_.bytes.size).getOrElse(0), 1) ++
          threshold_pk.map(_.bytes).getOrElse(ByteVector.empty) ++
          mPrefix ++
          ByteVector.fromLong(extra_in.map(_.length).getOrElse(0), 4) ++
          extra_in.getOrElse(ByteVector.empty) ++
          ByteVector.fromByte(i.toByte)
        val hash = hashFrostNonce(b)
        FieldElement.fromBytes(hash)
      }
      .toVector
    require(!preimages.contains(FieldElement.zero),
            "Derived nonce preimage cannot be zero")
    val r1: ECPublicKey = CryptoParams.getG.multiply(preimages.head)
    val r2: ECPublicKey = CryptoParams.getG.multiply(preimages(1))
    require(CryptoUtil.decodePoint(r1) != SecpPointInfinity)
    require(CryptoUtil.decodePoint(r2) != SecpPointInfinity)
    val pubnonce = r1.bytes ++ r2.bytes
    val secnonce = preimages.head.bytes ++ preimages(1).bytes
    (FrostNoncePriv(secnonce), FrostNoncePub(pubnonce))
  }

  def aggregateNonces(
      pubnonces: Vector[ByteVector],
      participantIdentifiers: Vector[Int]): FrostNoncePub = {
    require(
      pubnonces.length == participantIdentifiers.length,
      s"Number of pubnonces (${pubnonces.length}) must match number of participant identifiers (${participantIdentifiers.length})"
    )
    val zip = pubnonces.zip(participantIdentifiers)
    val aggPoints = 0.until(2).map { j =>
      val points = zip.zipWithIndex.map { case (z, idx) =>
        val nonce = if (j == 0) {
          z._1.take(33)
        } else {
          z._1.takeRight(33)
        }
        try {
          val pubkey = ECPublicKey.fromBytes(nonce)
          val point = SecpPoint.fromPublicKey(pubkey)
          point
        } catch {
          case _: Throwable =>
            throw new IllegalArgumentException(
              s"Invalid nonce for participant identifier ${z._2} at index $idx: ${nonce.toHex}")
        }
      }
      val agg: SecpPoint = points.reduce[SecpPoint] { (a, b) =>
        a.add(b)
      }
      agg
    }
    FrostNoncePub(aggPoints(0).bytes ++ aggPoints(1).bytes)
  }

  /** Computes the FROST Lagrange coefficient \(\lambda_{myId}\) for combining
    * secret shares.
    *
    * The coefficient computed is: `lambda_myId = product_{j != myId} (id_j + 1)
    * / (id_j - myId)`
    *
    * All arithmetic is performed in the prime field represented by
    * `FieldElement`.
    *
    * Preconditions:
    *   - `ids` contains `myId`.
    *   - `ids` contains no duplicates.
    *   - all identifiers in `ids` and `myId` are non\-negative.
    *
    * Throws an IllegalArgumentException if any precondition fails or if the
    * denominator evaluates to zero (so the multiplicative inverse does not
    * exist in the field).
    *
    * @param ids
    *   Vector of participant identifiers (distinct, non\-negative).
    * @param myId
    *   Identifier for which to compute the Lagrange coefficient.
    * @return
    *   The Lagrange coefficient as a `FieldElement`.
    */
  def deriveInterpolatingValue(ids: Vector[Int], myId: Int): FieldElement = {
    require(ids.contains(myId),
            s"My id $myId must be in the list of participant ids: $ids")
    require(ids.distinct.length == ids.length,
            s"ids must not contain duplicates, ids=$ids")
    require(0 <= myId && myId < 4294967296L,
            s"myId must be in the range [2, 2^32 - 1], got: $myId")
    val initNum: FieldElement = FieldElement.one
    val initDenom: FieldElement = FieldElement.one
    val (num, denom) = ids.foldLeft((initNum, initDenom)) {
      case ((num, denom), id) =>
        if (id == myId) {
          (num, denom) // skip
        } else {
          val n = num.multiply(FieldElement(id + 1))
          val d = denom.multiply(FieldElement(id - myId))
          (n, d)
        }
    }
    num.multiply(denom.inverse)
  }

  def sign(
      secNonce: ByteVector,
      secShare: FieldElement,
      myId: Long,
      sessionContext: FrostSessionContext): FieldElement = {
    ???
  }
}
