package org.bitcoins.crypto.frost

import org.bitcoins.crypto.*
import org.bitcoins.crypto.musig.{Neg, Pos}
import scodec.bits.ByteVector

import java.math.BigInteger

object FrostUtil {

  def hashFrostAux(bytes: ByteVector): ByteVector = {
    CryptoUtil.taggedSha256(bytes, "FROST/aux").bytes
  }

  def hashFrostNonce(bytes: ByteVector): ByteVector = {
    CryptoUtil.taggedSha256(bytes, "FROST/nonce").bytes
  }

  def hashFrostNonceCoef(bytes: ByteVector): ByteVector = {
    CryptoUtil.taggedSha256(bytes, "FROST/noncecoef").bytes
  }

  def hashFrostDeterministicNonce(bytes: ByteVector): ByteVector = {
    CryptoUtil.taggedSha256(bytes, "FROST/deterministic/nonce").bytes
  }

  def hashFrostCoeffGen(bytes: ByteVector): ByteVector = {
    CryptoUtil.taggedSha256(bytes, "FROST/coeffgen").bytes
  }

  def hashFrostDeterministicNonce(
      secshare: FieldElement,
      aggOtherNonce: FrostNoncePub,
      tweakThresholdPubKey: XOnlyPubKey,
      message: ByteVector,
      i: Byte): ByteVector = {
    val b = secshare.bytes ++
      aggOtherNonce.bytes ++
      tweakThresholdPubKey.bytes ++
      ByteVector.fromLong(message.length, 8) ++
      message ++
      ByteVector.fromByte(i)
    hashFrostDeterministicNonce(b)
  }

  def nonceGen(
      rand: ByteVector,
      secshare: Option[FieldElement],
      pubshare: Option[ECPublicKey],
      threshold_pk: Option[XOnlyPubKey],
      message: Option[ByteVector],
      extra_in: Option[ByteVector]): (FrostNoncePriv, FrostNoncePub) = {
    val randPrime = secshare match {
      case Some(sec) => sec.bytes.xor(hashFrostAux(rand))
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
      pubnonces: Vector[FrostNoncePub],
      participantIdentifiers: Vector[Long]): FrostNoncePub = {
    require(
      pubnonces.length == participantIdentifiers.length,
      s"Number of pubnonces (${pubnonces.length}) must match number of participant identifiers (${participantIdentifiers.length})"
    )
    val zip = pubnonces.zip(participantIdentifiers)
    val aggPoints = 0.until(2).map { j =>
      val points = zip.zipWithIndex.map { case (z, idx) =>
        val nonce = if (j == 0) {
          z._1.r1
        } else {
          z._1.r2
        }
        require(
          nonce != SecpPointInfinity,
          s"Nonce at index $idx for participant id ${z._2} is the point at infinity")
        nonce
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
  def deriveInterpolatingValue(ids: Vector[Long], myId: Long): FieldElement = {
    require(ids.contains(myId),
            s"My id $myId must be in the list of participant ids: $ids")
    require(ids.distinct.length == ids.length,
            s"ids must not contain duplicates, ids=$ids")
    require(0 <= myId && myId < 4294967296L,
            s"myId must be in the range [0, 2^32 - 1], got: $myId")
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
      secNonce: FrostNoncePriv,
      secShare: FieldElement,
      signerId: Long,
      sessionContext: FrostSessionContext): FieldElement = {
    require(
      sessionContext.signingContext.participantIds.contains(signerId),
      s"My id $signerId must be in the signing context ids: ${sessionContext.signingContext.participantIds}"
    )
    val values = sessionContext.getSessionValues
    val (k1, k2) = values.R.toPublicKey.parity match {
      case EvenParity => (secNonce.k1, secNonce.k2)
      case OddParity  => (secNonce.k1.negate, secNonce.k2.negate)
    }
    require(secShare != FieldElement.zero,
            s"Secret share for participant id $signerId cannot be zero")
    val pubshare = CryptoParams.getG.multiply(secShare)
    require(
      values.pubshares.contains(pubshare),
      s"Public share $pubshare derived from secret share does not exist in the session context pubshares: ${values.pubshares}"
    )
    val lambda =
      deriveInterpolatingValue(sessionContext.signingContext.participantIds,
                               signerId)
    val g = values.Q.toPublicKey.parity match {
      case EvenParity => Pos
      case OddParity  => Neg
    }

    val d = values.gacc
      .multiply(g)
      .modify(secShare)

    // s = k1 + b · k2 + e · λ · d
    val s = k1.fieldElement
      .add(k2.fieldElement.multiply(values.b))
      .add(values.e.multiply(lambda).multiply(d))
    val pubnonce = secNonce.toNoncePub
    val verified = partialSigVerifyInternal(
      partialSig = s,
      signerId = signerId,
      pubNonce = pubnonce,
      pubshare = pubshare,
      sessionCtx = sessionContext
    )
    require(
      verified,
      s"Computed partial signature $s failed verification"
    )
    s
  }

  def partialSigVerify(
      partialSig: FieldElement,
      pubnonces: Vector[FrostNoncePub],
      signersContext: FrostSigningContext,
      tweaks: Vector[FieldElement],
      isXonlyT: Vector[Boolean],
      message: ByteVector,
      signerId: Long): Boolean = {
    require(
      signersContext.participantIds.contains(signerId),
      s"Signer id $signerId must be in the signing context ids: ${signersContext.participantIds}")
    // val u = signersContext.u
    val ids = signersContext.participantIds
    val pubshares = signersContext.pubshares
    // val thresholdPubKey = signersContext.thresholdPubKey
    val aggNonce = FrostUtil.aggregateNonces(
      pubnonces,
      ids
    )
    val sessionCtx = FrostSessionContext(
      signingContext = signersContext,
      aggNonce = aggNonce,
      tweaks = tweaks,
      isXOnly = isXonlyT,
      message = message
    )
    partialSigVerifyInternal(
      partialSig,
      signerId = signerId,
      pubNonce = pubnonces(ids.indexOf(signerId)),
      pubshare = pubshares(ids.indexOf(signerId)),
      sessionCtx = sessionCtx
    )
  }

  def partialSigVerifyInternal(
      partialSig: FieldElement,
      signerId: Long,
      pubNonce: FrostNoncePub,
      pubshare: ECPublicKey,
      sessionCtx: FrostSessionContext): Boolean = {
    val ids = sessionCtx.signingContext.participantIds
    val pubshares = sessionCtx.signingContext.pubshares
    require(ids.contains(signerId),
            s"Signer id $signerId must be in the signing context ids: $ids")
    require(
      pubshares.contains(pubshare),
      s"Public share $pubshare must be in the signing context pubshares: $pubshares")
    val values = sessionCtx.getSessionValues
    val rePrime = pubNonce.r1
      .add(pubNonce.r2.multiply(values.b))
    val re = values.R.toPublicKey.parity match {
      case EvenParity => rePrime
      case OddParity  => rePrime.negate
    }
    val lambda = deriveInterpolatingValue(ids, signerId)
    val g = values.Q.toPublicKey.parity match {
      case EvenParity => Pos
      case OddParity  => Neg
    }
    val gPrime = values.gacc
      .multiply(g)
    val actualS = CryptoParams.getG.multiply(partialSig).toPoint
    val inner = pubshare
      .multiply(values.e.multiply(lambda))
    val expectedS: SecpPoint = re
      .add(gPrime.modify(inner).toPoint)
    expectedS == actualS
  }

  def partialSigAgg(
      partialSigs: Vector[FieldElement],
      ids: Vector[Long],
      sessionContext: FrostSessionContext): SchnorrDigitalSignature = {
    require(
      partialSigs.length == ids.length,
      s"Number of partial signatures (${partialSigs.length}) must match number of participant identifiers (${ids.length})"
    )
    val values = sessionContext.getSessionValues
    val sAgg = partialSigs.foldLeft(FieldElement.zero) { case (acc, sig) =>
      acc.add(sig)
    }
    val g = values.Q.toPublicKey.parity match {
      case EvenParity => Pos
      case OddParity  => Neg
    }
    // s + e * g * tacc
    val eGTacc = g
      .modify(values.tacc)
      .multiply(values.e)
    val s = sAgg
      .add(eGTacc)
    val schnorrNonce = SchnorrNonce.apply(values.R.x)
    new SchnorrDigitalSignature(schnorrNonce, s, hashTypeOpt = None)
  }

  val COORDINATOR_ID: Long = 1337L

  def deterministicSign(
      secshare: FieldElement,
      signerId: Long,
      aggOtherNonce: FrostNoncePub,
      signersContext: FrostSigningContext,
      tweaks: Vector[FieldElement],
      isXOnly: Vector[Boolean],
      message: ByteVector,
      auxRandOpt: Option[ByteVector]): (FrostNoncePub, FieldElement) = {
    val secsharePrime = auxRandOpt match {
      case Some(auxRand) =>
        val b = secshare.bytes.xor(hashFrostAux(auxRand))
        FieldElement.fromBytes(b)
      case None => secshare
    }
    val tweakedThresholdPubKey = FrostTweakContext.calculateTweakedKey(
      signersContext.thresholdPubKey,
      tweaks,
      isXOnly)
    val preimages: Vector[FieldElement] = 0
      .to(1)
      .map { i =>
        hashFrostDeterministicNonce(secsharePrime,
                                    aggOtherNonce,
                                    tweakedThresholdPubKey.toXOnly,
                                    message,
                                    i.toByte)
      }
      .toVector
      .map(FieldElement.fromBytes)
    require(!preimages.contains(FieldElement.zero),
            "Derived nonce preimage cannot be zero")
    val r1: ECPublicKey = CryptoParams.getG.multiply(preimages.head)
    val r2: ECPublicKey = CryptoParams.getG.multiply(preimages(1))
    require(CryptoUtil.decodePoint(r1) != SecpPointInfinity)
    require(CryptoUtil.decodePoint(r2) != SecpPointInfinity)
    val pubNonce = FrostNoncePub(r1.bytes ++ r2.bytes)

    val secNonce = FrostNoncePriv(preimages.head.bytes ++ preimages(1).bytes)
    val aggNonce = aggregateNonces(Vector(pubNonce, aggOtherNonce),
                                   Vector(signerId, COORDINATOR_ID))
    val sessionCtx = FrostSessionContext(
      signingContext = signersContext,
      aggNonce = aggNonce,
      tweaks = tweaks,
      isXOnly = isXOnly,
      message = message
    )
    val sig = sign(secNonce = secNonce,
                   secShare = secshare,
                   signerId = signerId,
                   sessionContext = sessionCtx)
    (pubNonce, sig)
  }

  /** https://github.com/jesseposner/secp256k1-zkp/blob/b14bad4b87aa267d216d1bc19f6a3fa7ca2ae366/src/modules/frost/keygen_impl.h#L155
    *
    * @param seed
    * @param threshold
    * @param numShares
    * @return
    */
  def generateShares(
      seed: ECPrivateKey,
      threshold: Int,
      numShares: Int): FrostShareGenResult = {
    require(threshold > 1, s"Threshold must be > 1: $threshold")
    require(
      numShares >= threshold,
      s"Number of shares must be at least the threshold, got: $numShares with threshold $threshold")
    val vssPreimage =
      seed.bytes ++ ByteVector.fromLong(threshold, 8) ++ ByteVector.fromLong(
        numShares,
        8)
    val polygen = CryptoUtil.sha256(vssPreimage).bytes
    val vssCommitments = vssCommitment(polygen, threshold)
    require(
      vssCommitments.size == threshold,
      s"Number of commitments must match threshold: " +
        s"got ${vssCommitments.size} commitments for threshold $threshold")
    val idSharesTuple: Vector[(Long, FieldElement)] = 0L
      .until(numShares)
      .map { i =>
        val id = i + 1
        val share = generateShare(polygen, threshold, id)
        (id, share)
      }
      .toVector
    FrostShareGenResult(
      ids = idSharesTuple.map(_._1),
      shares = idSharesTuple.map(_._2),
      commitments = vssCommitments
    )
  }

  /** https://github.com/jesseposner/secp256k1-zkp/blob/frost-trusted-dealer/src/modules/frost/keygen_impl.h#L130
    * @param polygen
    * @param threshold
    * @param id
    * @return
    */
  def generateShare(
      seed: ByteVector,
      threshold: Int,
      id: Long): FieldElement = {
    var shareI = FieldElement.zero
    deriveCoefficients(seed, threshold).reverse.foreach { coeff =>
      shareI = shareI.multiply(FieldElement(id))
      shareI = shareI.add(coeff)
    }
    shareI
  }

  def vssCommitment(
      polygen: ByteVector,
      threshold: Int): Vector[ECPublicKey] = {
    deriveCoefficients(polygen, threshold).map { coeff =>
      CryptoParams.getG.multiply(coeff)
    }
  }

  def deriveCoefficients(
      polygen: ByteVector,
      threshold: Int): Vector[FieldElement] = {
    0.until(threshold)
      .map(deriveCoefficient(polygen, _))
      .toVector
  }

  def deriveCoefficient(seed: ByteVector, idx: Int): FieldElement = {
    val coeffPreimage = seed ++ ByteVector.fromLong(idx, 8)
    val coeff = hashFrostCoeffGen(coeffPreimage)
    FieldElement.fromBytes(coeff)
  }

  def secShareVerify(): Boolean = ???

  def vssVerify(
      share: FieldElement,
      id: Long,
      commitments: Vector[ECPublicKey]): Boolean = {
    require(id > 0, s"Identifier must be positive, got: $id")
    val lhs = CryptoParams.getG.multiply(share)
    val rhs: SecpPoint =
      commitments.zipWithIndex.foldLeft[SecpPoint](SecpPointInfinity) {
        case (acc, (commitment, j)) =>
          val y = FieldElement(id).pow(BigInteger.valueOf(j))
          val x = commitment.multiply(y)
          acc.add(x.toPoint)
      }
    lhs.toPoint == rhs
  }

  def computeThresholdPubKey(
      pubshares: Vector[ECPublicKey],
      ids: Vector[Long]): ECPublicKey = {
    var q: SecpPoint = SecpPointInfinity
    pubshares.zipWithIndex.foreach { case (p, idx) =>
      val myId = ids(idx)
      val interpolation =
        FrostUtil.deriveInterpolatingValue(ids = ids, myId = myId)
      val x = p.toPoint.multiply(interpolation)
      q = q.add(x)
    }
    q match {
      case SecpPointInfinity =>
        throw new IllegalArgumentException(
          s"Computed threshold pubkey is point at infinity")
      case p: SecpPointFinite => p.toPublicKey
    }
  }
}
