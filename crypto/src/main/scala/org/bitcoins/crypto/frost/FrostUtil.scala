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

  /** Generates a FROST nonce pair (secret and public).
    *
    * This function generates two secret nonces (k1, k2) and their corresponding
    * public nonces (R1, R2) for use in a FROST signing session. The nonces are
    * derived deterministically from the provided inputs using tagged hashes.
    *
    * The nonces can be optionally bound to:
    *   - The signer's secret share (for additional security)
    *   - The signer's public share
    *   - The threshold public key (to bind to a specific signing group)
    *   - A specific message (to prevent nonce reuse across messages)
    *   - Extra input data
    *
    * Security note: The `rand` parameter must be 32 bytes of cryptographically
    * secure random data. If `secshare` is provided, the randomness is mixed
    * with the secret share using the auxiliary hash to protect against
    * side-channel attacks.
    *
    * @param rand
    *   32 bytes of random data (must be cryptographically secure)
    * @param secshare
    *   optional secret share to mix with randomness (recommended for security)
    * @param pubshare
    *   optional public share to bind nonces to a specific participant
    * @param threshold_pk
    *   optional threshold public key to bind nonces to a signing group
    * @param message
    *   optional message to bind nonces to (prevents reuse across messages)
    * @param extra_in
    *   optional extra input data for domain separation
    * @return
    *   a tuple of (secret nonce, public nonce)
    * @throws IllegalArgumentException
    *   if derived nonce preimages are zero or points at infinity
    */
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

  /** Aggregates public nonces from multiple participants.
    *
    * This function combines the public nonces from all signing participants
    * into a single aggregated nonce. Each participant contributes two nonce
    * points (R1_i, R2_i), and this function computes the aggregated nonces as:
    *   - R1 = sum(R1_i) for all participants i
    *   - R2 = sum(R2_i) for all participants i
    *
    * The aggregated nonce is used in the session context to compute the binding
    * factor `b` and the effective nonce point R = R1 + b·R2.
    *
    * @param pubnonces
    *   vector of public nonces from each participant
    * @param participantIdentifiers
    *   vector of participant IDs corresponding to each nonce (must be same
    *   length as pubnonces)
    * @return
    *   the aggregated public nonce containing (R1, R2)
    * @throws IllegalArgumentException
    *   if lengths don't match or any nonce point is at infinity
    */
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
    * The coefficient computed is: lambda_myId = product_{j != myId} (id_j + 1)
    * \/ (id_j - myId)
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

  /** Generates a FROST partial signature.
    *
    * This function produces a partial signature for a single participant in a
    * FROST signing session. The partial signature is computed as:
    *
    * s = k1 + b·k2 + e·λ·d
    *
    * where:
    *   - k1, k2 are the signer's secret nonces (possibly negated based on R's
    *     parity)
    *   - b is the nonce binding factor
    *   - e is the challenge scalar
    *   - λ (lambda) is the Lagrange interpolation coefficient for this signer
    *   - d is the adjusted secret share (gacc·g·secShare)
    *
    * The function internally verifies the computed partial signature before
    * returning it.
    *
    * @param secNonce
    *   the signer's secret nonce pair (k1, k2)
    * @param secShare
    *   the signer's secret share (must be non-zero)
    * @param signerId
    *   the identifier of this signer (must be in the session context)
    * @param sessionContext
    *   the signing session context containing aggregated nonces, tweaks, and
    *   message
    * @return
    *   the partial signature as a FieldElement
    * @throws IllegalArgumentException
    *   if signerId is invalid, secShare is zero, derived pubshare doesn't match
    *   context, or signature verification fails
    */
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

  /** Verifies a FROST partial signature.
    *
    * This function verifies that a partial signature from a specific signer is
    * valid. It reconstructs the session context from the provided inputs,
    * aggregates the nonces, and then calls the internal verification function.
    *
    * The verification checks that: s·G = R_i + e·λ·gacc·g·X_i
    *
    * where:
    *   - s is the partial signature
    *   - R_i is the signer's contribution to the nonce (R1_i + b·R2_i)
    *   - e is the challenge scalar
    *   - λ is the Lagrange coefficient for this signer
    *   - X_i is the signer's public share
    *
    * @param partialSig
    *   the partial signature to verify
    * @param pubnonces
    *   vector of all participants' public nonces
    * @param signersContext
    *   the signing context with participant information
    * @param tweaks
    *   optional tweaks applied to the threshold public key
    * @param isXonlyT
    *   flags indicating which tweaks are x-only
    * @param message
    *   the message that was signed
    * @param signerId
    *   the identifier of the signer whose signature is being verified
    * @return
    *   true if the partial signature is valid, false otherwise
    * @throws IllegalArgumentException
    *   if signerId is not in the signing context
    */
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

  /** Internal partial signature verification (used by sign and
    * partialSigVerify).
    *
    * This function performs the actual verification of a partial signature
    * using the session context that has already been constructed. It checks the
    * signing equation:
    *
    * s·G = R_i + e·λ·gacc·g·X_i
    *
    * where:
    *   - s is the partial signature scalar
    *   - G is the generator point
    *   - R_i = R1_i + b·R2_i (possibly negated based on aggregate R's parity)
    *   - e is the challenge scalar
    *   - λ is the Lagrange coefficient
    *   - gacc and g are parity accumulators
    *   - X_i is the signer's public share
    *
    * @param partialSig
    *   the partial signature scalar to verify
    * @param signerId
    *   the identifier of the signer
    * @param pubNonce
    *   the signer's public nonce (R1_i, R2_i)
    * @param pubshare
    *   the signer's public share (X_i)
    * @param sessionCtx
    *   the session context with all session values
    * @return
    *   true if the partial signature is valid, false otherwise
    * @throws IllegalArgumentException
    *   if signerId or pubshare are not in the session context
    */
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

  /** Aggregates partial signatures into a final Schnorr signature.
    *
    * This function combines the partial signatures from all signing
    * participants into a complete Schnorr signature. The aggregation computes:
    *
    * s = sum(s_i) + e·g·tacc
    *
    * where:
    *   - s_i are the partial signatures
    *   - e is the challenge scalar
    *   - g is the parity multiplier for the aggregate public key
    *   - tacc is the tweak accumulator
    *
    * The final signature is (R, s) where R is the x-only nonce point from the
    * session context.
    *
    * @param partialSigs
    *   vector of partial signatures from each participant
    * @param ids
    *   vector of participant identifiers (same order as partialSigs)
    * @param sessionContext
    *   the session context containing session values (R, e, tweaks, etc.)
    * @return
    *   a complete BIP-340 Schnorr signature
    * @throws IllegalArgumentException
    *   if the number of partial signatures doesn't match the number of IDs
    */
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

  /** Fixed participant ID used to represent the coordinator in deterministic
    * signing.
    *
    * In FROST deterministic signing, the coordinator aggregates nonces from
    * other participants but doesn't contribute a secret share. This fixed ID
    * (1337) is used to identify the coordinator's nonce contribution when
    * aggregating nonces.
    */
  val COORDINATOR_ID: Long = 1337L

  /** Generates a deterministic partial signature for stateless signing.
    *
    * This function enables a signer to generate both their nonce and partial
    * signature deterministically from their secret share and the coordinator's
    * aggregated nonce. This is useful for stateless signing where the signer
    * doesn't need to store generated nonces between rounds.
    *
    * The deterministic nonces are derived by hashing:
    *   - The secret share (optionally mixed with auxiliary randomness)
    *   - The aggregated nonce from other participants
    *   - The tweaked threshold public key
    *   - The message
    *
    * This ensures the nonces are deterministic but unique per message and
    * signing session.
    *
    * @param secshare
    *   the signer's secret share
    * @param signerId
    *   the identifier of this signer
    * @param aggOtherNonce
    *   the aggregated public nonce from other participants (typically the
    *   coordinator)
    * @param signersContext
    *   the signing context with participant information
    * @param tweaks
    *   optional tweaks to apply to the threshold public key
    * @param isXOnly
    *   flags indicating which tweaks are x-only
    * @param message
    *   the message to be signed
    * @param auxRandOpt
    *   optional auxiliary randomness to mix with the secret share (recommended
    *   for additional security)
    * @return
    *   a tuple of (public nonce, partial signature)
    * @throws IllegalArgumentException
    *   if derived nonces are zero, points at infinity, or signature
    *   verification fails
    */
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

  /** Generates FROST secret shares using a trusted dealer.
    *
    * This function implements the trusted dealer key generation for FROST. The
    * dealer generates:
    *   - Secret shares for n participants using polynomial secret sharing
    *   - VSS (Verifiable Secret Sharing) commitments that allow participants to
    *     verify their shares
    *   - A threshold public key representing the aggregate signing key
    *
    * The polynomial has degree t-1, which means any t participants can
    * collaborate to produce a signature, but fewer than t cannot.
    *
    * Each participant receives their secret share and can verify it against the
    * public commitments using `vssVerify`.
    *
    * Security note: This is a trusted dealer setup where the dealer knows all
    * secret shares. For production use, consider using a distributed key
    * generation (DKG) protocol instead.
    *
    * @param seed
    *   the secret seed used to generate the polynomial (must be kept secret by
    *   dealer)
    * @param threshold
    *   the minimum number of signers required (t, must be > 1)
    * @param numShares
    *   the total number of shares to generate (n, must be >= threshold)
    * @return
    *   FrostShareGenResult containing participant IDs, secret shares, and VSS
    *   commitments
    * @throws IllegalArgumentException
    *   if threshold <= 1 or numShares < threshold
    * @see
    *   [[https://github.com/jesseposner/secp256k1-zkp/blob/b14bad4b87aa267d216d1bc19f6a3fa7ca2ae366/src/modules/frost/keygen_impl.h#L155]]
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

  /** Generates a single secret share using polynomial evaluation.
    *
    * This function evaluates the secret sharing polynomial at a specific
    * participant ID to generate their secret share. The polynomial is of degree
    * t-1, where t is the threshold.
    *
    * The share is computed as: share_i = a_0 + a_1·i + a_2·i^2 + ... +
    * a_{t-1}·i^{t-1}
    *
    * where a_0, a_1, ..., a_{t-1} are the polynomial coefficients derived from
    * the seed.
    *
    * @param seed
    *   the seed bytes used to derive polynomial coefficients
    * @param threshold
    *   the threshold (determines polynomial degree = threshold - 1)
    * @param id
    *   the participant identifier (must be positive)
    * @return
    *   the secret share for this participant as a FieldElement
    * @see
    *   [[https://github.com/jesseposner/secp256k1-zkp/blob/frost-trusted-dealer/src/modules/frost/keygen_impl.h#L130]]
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

  /** Generates VSS (Verifiable Secret Sharing) commitments.
    *
    * This function computes the public commitments for each polynomial
    * coefficient. Each commitment is C_j = a_j·G, where a_j is the j-th
    * coefficient and G is the generator point.
    *
    * These commitments allow participants to verify their secret shares without
    * revealing the shares themselves.
    *
    * @param polygen
    *   the seed bytes used to derive polynomial coefficients
    * @param threshold
    *   the threshold (determines number of commitments = threshold)
    * @return
    *   vector of public key commitments (one per polynomial coefficient)
    */
  def vssCommitment(
      polygen: ByteVector,
      threshold: Int): Vector[ECPublicKey] = {
    deriveCoefficients(polygen, threshold).map { coeff =>
      CryptoParams.getG.multiply(coeff)
    }
  }

  /** Derives all polynomial coefficients from a seed.
    *
    * This function generates t polynomial coefficients (a_0, a_1, ..., a_{t-1})
    * by hashing the seed with different indices. These coefficients define the
    * secret sharing polynomial.
    *
    * @param polygen
    *   the seed bytes for coefficient derivation
    * @param threshold
    *   the threshold (number of coefficients to generate)
    * @return
    *   vector of t field element coefficients
    */
  def deriveCoefficients(
      polygen: ByteVector,
      threshold: Int): Vector[FieldElement] = {
    0.until(threshold)
      .map(deriveCoefficient(polygen, _))
      .toVector
  }

  /** Derives a single polynomial coefficient from a seed and index.
    *
    * This function generates one coefficient by hashing the seed concatenated
    * with the coefficient index. Each coefficient is derived independently
    * using the FROST coefficient generation hash.
    *
    * @param seed
    *   the seed bytes for coefficient derivation
    * @param idx
    *   the coefficient index (0 for constant term, 1 for linear term, etc.)
    * @return
    *   the coefficient as a FieldElement
    */
  def deriveCoefficient(seed: ByteVector, idx: Int): FieldElement = {
    val coeffPreimage = seed ++ ByteVector.fromLong(idx, 8)
    val coeff = hashFrostCoeffGen(coeffPreimage)
    FieldElement.fromBytes(coeff)
  }

  /** Verifies a secret share against VSS commitments.
    *
    * This function allows a participant to verify that their secret share is
    * consistent with the public VSS commitments. It checks the equation:
    *
    * share·G = sum(C_j · id^j) for j = 0 to t-1
    *
    * where:
    *   - share is the secret share
    *   - G is the generator point
    *   - C_j are the VSS commitments
    *   - id is the participant identifier
    *   - t is the threshold
    *
    * This ensures the dealer generated the share correctly without revealing
    * the share itself.
    *
    * @param share
    *   the secret share to verify
    * @param id
    *   the participant identifier (must be positive)
    * @param commitments
    *   the VSS commitments from the dealer
    * @return
    *   true if the share is valid, false otherwise
    * @throws IllegalArgumentException
    *   if id is not positive
    */
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

  /** Computes the threshold public key from participant public shares.
    *
    * This function derives the aggregate (threshold) public key from the
    * individual public shares of the participants. The threshold public key is
    * computed using Lagrange interpolation:
    *
    * Q = sum(λ_i · X_i) for all participants i
    *
    * where:
    *   - λ_i is the Lagrange interpolation coefficient for participant i
    *   - X_i is participant i's public share (X_i = share_i · G)
    *
    * This public key represents the aggregate key that can be used to verify
    * FROST signatures. Any t-of-n participants can collaborate to sign with
    * this public key.
    *
    * @param pubshares
    *   vector of public shares from each participant
    * @param ids
    *   vector of participant identifiers (same order as pubshares)
    * @return
    *   the threshold public key
    * @throws IllegalArgumentException
    *   if the computed key is the point at infinity
    */
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
