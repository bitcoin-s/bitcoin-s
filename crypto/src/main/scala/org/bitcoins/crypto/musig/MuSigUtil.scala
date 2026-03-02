package org.bitcoins.crypto.musig

import org.bitcoins.crypto.*
import scodec.bits.ByteVector

/** Utilities for MuSig2 operations and BIP-0327-related behaviour.
  *
  * This object implements nonce generation, nonce aggregation, partial
  * signature generation and verification, and deterministic signing helpers
  * used by the MuSig2 protocol. The implementation follows the BIP-0327
  * ("MuSig2") reference where applicable; see:
  *
  *   - BIP-0327: https://github.com/bitcoin/bips/blob/master/bip-0327.mediawiki
  *
  * Important details and conventions implemented here:
  *   - Nonces are represented as pairs (r1, r2) and are collapsed using a
  *     polynomial sum with a session scalar b as described in the BIP.
  *   - The point-at-infinity serialization is encoded as 33 zero bytes in
  *     `MuSigNoncePub` (see `MuSigNoncePub.infPtBytes`).
  *   - Parity handling for public keys and the aggregated nonce R is handled
  *     via `ParityMultiplier` and conditional negation when required. This
  *     matches the reference implementation behavior where sign adjustments are
  *     applied during both signing and verification so that stored original
  *     public keys remain unmodified.
  *   - The functions here are low-level helpers; higher-level session
  *     construction and key-aggregation logic is in `MuSigSessionContext` and
  *     `KeySet`.
  */
object MuSigUtil {

  /** Number of nonce components used per signer. MuSig2 uses two independent
    * ephemeral nonces per signer (r1, r2).
    */
  val nonceNum: Int = 2

  /** Tagged hash for key aggregation list digest used when computing key
    * aggregation coefficients (BIP-0327: "KeyAgg list").
    */
  def aggListHash(bytes: ByteVector): ByteVector = {
    CryptoUtil.taggedSha256(bytes, "KeyAgg list").bytes
  }

  /** Tagged hash used to derive the per-key coefficient in key aggregation
    * (BIP-0327: "KeyAgg coefficient").
    */
  def aggCoefHash(bytes: ByteVector): ByteVector = {
    CryptoUtil.taggedSha256(bytes, "KeyAgg coefficient").bytes
  }

  /** Tagged hash for MuSig nonce derivation (BIP-0327: "MuSig/nonce"). */
  def nonHash(bytes: ByteVector): ByteVector = {
    CryptoUtil.taggedSha256(bytes, "MuSig/nonce").bytes
  }

  /** Tagged hash for nonce coefficient derivation (BIP-0327:
    * "MuSig/noncecoef").
    */
  def nonCoefHash(bytes: ByteVector): ByteVector = {
    CryptoUtil.taggedSha256(bytes, "MuSig/noncecoef").bytes
  }

  /** Tagged hash for auxiliary randomness used in nonce generation (BIP-0327:
    * "MuSig/aux").
    */
  def auxHash(bytes: ByteVector): ByteVector = {
    CryptoUtil.taggedSha256(bytes, "MuSig/aux").bytes
  }

  /** Deterministic nonce hash used by the deterministic signing helper
    * (BIP-0327 reference tag "MuSig/deterministic/nonce").
    */
  def muSigDeterministicNonceHash(bytes: ByteVector): ByteVector = {
    CryptoUtil.taggedSha256(bytes, "MuSig/deterministic/nonce").bytes
  }

  /** Generic polynomial summation of nonce components.
    *
    * The nonce pair (r0, r1, ..., rN) is collapsed to a single point or scalar
    * by summing `r_i * b^i` for i from 0 to N-1 as described in BIP-0327. This
    * helper abstracts the accumulation pattern and is parameterized over the
    * element type T (which may be a field element or an EC point
    * representation).
    *
    * @param nonces
    *   the vector of nonces to sum (length must equal nonceNum)
    * @param b
    *   session scalar used as the polynomial base
    * @param add
    *   function to add two T elements
    * @param multiply
    *   function to multiply a T by a FieldElement
    * @param identity
    *   additive identity for T
    * @return
    *   resulting collapsed T (e.g., a FieldElement or SecpPoint)
    */
  private[musig] def nonceSum[T](
      nonces: Vector[T],
      b: FieldElement,
      add: (T, T) => T,
      multiply: (T, FieldElement) => T,
      identity: T): T = {
    nonces
      .foldLeft((FieldElement.one, identity)) { case ((pow, sumSoFar), nonce) =>
        val prod = multiply(nonce, pow)

        (pow.multiply(b), add(sumSoFar, prod))
      }
      ._2
  }

  /** Generate a MuSig nonce private pair deterministically from the provided
    * entropy and optional contextual inputs.
    *
    * This function implements the nonce derivation described in BIP-0327 where
    * the nonce generation input includes: per-signer randomness (preRand), the
    * signer's compressed public key, optionally the aggregated public key, an
    * optional message, and optional extra input. If a `privKeyOpt` is provided,
    * aux hashing is XORed with the private key bytes to provide domain
    * separation as in the reference.
    *
    * The returned `MuSigNoncePriv` contains two private nonces (k1,k2) and the
    * signer's compressed public key (so the nonce packet is self-contained for
    * verification/tweaks).
    *
    * @param preRand
    *   32 bytes of entropy (must be length 32)
    * @param publicKey
    *   signer's compressed public key (must be compressed)
    * @param privKeyOpt
    *   optional signer's private key used to mix into aux
    * @param aggPubKeyOpt
    *   optional aggregated public key for session binding
    * @param msgOpt
    *   optional message to bind to the nonce (encoded lengthwise)
    * @param extraInOpt
    *   optional additional bytes (maximum length = 4294967295 bytes)
    */
  def nonceGen(
      preRand: ByteVector,
      publicKey: ECPublicKey,
      privKeyOpt: Option[ECPrivateKey],
      aggPubKeyOpt: Option[SchnorrPublicKey],
      msgOpt: Option[ByteVector],
      extraInOpt: Option[ByteVector]): MuSigNoncePriv = {
    require(preRand.length == 32,
            s"32 bytes of entropy must be provided, found $preRand")
    require(
      extraInOpt.forall(_.length <= 4294967295L),
      "extraIn too long, its length must be represented by at most four bytes")
    require(publicKey.isCompressed, s"PublicKey must be compressed for MuSig2")
    def serializeWithLen(
        bytesOpt: Option[ByteVector],
        lengthSize: Int = 1): ByteVector = {
      bytesOpt match {
        case Some(bytes) =>
          ByteVector.fromLong(bytes.length, lengthSize) ++ bytes
        case None => ByteVector.fromLong(0, lengthSize)
      }
    }

    val rand = privKeyOpt match {
      case Some(privKey) => MuSigUtil.auxHash(preRand).xor(privKey.bytes)
      case None          => preRand
    }

    val publicKeyBytes = serializeWithLen(Some(publicKey.bytes))
    val aggPubKeyBytes = serializeWithLen(aggPubKeyOpt.map(_.bytes))
    // Match the Python reference: None -> 0x00, Some(msg) -> 0x01 || len(msg,8) || msg
    // Note: an explicit empty message (Some(ByteVector.empty)) must be encoded as
    // 0x01 followed by 8 zero bytes (length 0), which differs from None.
    val msgBytes = msgOpt match {
      case Some(m) =>
        ByteVector.fromByte(1) ++ ByteVector.fromLong(m.length, 8) ++ m
      case None => ByteVector.fromByte(0)
    }
    val extraInBytes = serializeWithLen(extraInOpt, lengthSize = 4)

    val privNonceKeys = 0.until(MuSigUtil.nonceNum).toVector.map { index =>
      val indexByte = ByteVector.fromByte(index.toByte)
      val preimage = rand ++ publicKeyBytes ++
        aggPubKeyBytes ++ msgBytes ++ extraInBytes ++ indexByte
      val noncePreBytes = MuSigUtil.nonHash(preimage)

      FieldElement(noncePreBytes).toPrivateKey
    }

    MuSigNoncePriv(privNonceKeys(0), privNonceKeys(1), publicKey)
  }

  /** Convenience overload of `nonceGen` that generates 32 bytes of randomness
    * internally.
    */
  def nonceGen(
      pk: ECPublicKey,
      privKeyOpt: Option[ECPrivateKey] = None,
      aggPubKeyOpt: Option[SchnorrPublicKey] = None,
      msgOpt: Option[ByteVector] = None,
      extraInOpt: Option[ByteVector] = None): MuSigNoncePriv = {
    val preRand = CryptoUtil.randomBytes(32)

    nonceGen(preRand, pk, privKeyOpt, aggPubKeyOpt, msgOpt, extraInOpt)
  }

  /** Produce a MuSig partial signature for a given signer using the provided
    * nonce pair, aggregate nonce, and private key. Returns the scalar partial
    * signature `s` (a FieldElement) corresponding to this signer's
    * contribution.
    *
    * The function performs the parity and coefficient adjustments described in
    * BIP-0327, including:
    *   - conditional negation of nonce private components when the final
    *     aggregated R requires it,
    *   - application of the session parity multiplier to the signer's private
    *     key contribution, and
    *   - verification of the generated partial signature via
    *     `partialSigVerifyInternal` as an internal consistency check.
    *
    * Note: this is a low-level helper that expects the `aggNoncePub` to be the
    * aggregated public nonces for the session and `keySet` to represent the
    * signers participating in the session.
    */
  def sign(
      noncePriv: MuSigNoncePriv,
      aggNoncePub: MuSigNoncePub,
      privKey: ECPrivateKey,
      message: ByteVector,
      keySet: KeySet): FieldElement = {
    val signingSession =
      MuSigSessionContext(aggNoncePub, keySet, message)
    sign(noncePriv, privKey, signingSession)
  }

  /** Core signing routine used by the deterministic and non-deterministic sign
    * helpers. Implements the signing arithmetic from BIP-0327 while leaving
    * session construction to the caller.
    *
    * Preconditions:
    *   - the provided `noncePriv.publicKey` must equal `privKey.publicKey` as
    *     the nonce must be bound to the signer key used for signing.
    */
  def sign(
      noncePriv: MuSigNoncePriv,
      privKey: ECPrivateKey,
      signingSession: MuSigSessionContext): FieldElement = {
    val pubKey = privKey.publicKey
    if (pubKey != noncePriv.publicKey) {
      throw new IllegalArgumentException(
        "Nonce private key must be derived from the same public key, got " + pubKey.toString + " and " + noncePriv.publicKey.toString)
    }
    val values = signingSession.getSessionValues
    val keySet = signingSession.keySet
    val coef = keySet.getSessionKeyAggCoeff(signingSession, pubKey)
    val e = values.e
    val b = values.b

    val adjustedNoncePriv = values.R.toPublicKey.parity match {
      case EvenParity => noncePriv
      case OddParity =>
        noncePriv.negate
    }

    val g = ParityMultiplier.fromParity(values.Q.toPublicKey.parity)

    val adjustedPrivKey = values.gacc
      .multiply(g)
      .modify(privKey.fieldElement)

    val privNonceSum = adjustedNoncePriv.sumToKey(b)

    val s = adjustedPrivKey
      .multiply(e)
      .multiply(coef)
      .add(privNonceSum)

    val verified = partialSigVerifyInternal(s,
                                            Vector(noncePriv.toNoncePub),
                                            pubKey,
                                            signingSession)

    require(
      verified,
      s"Failed partialSigVerifyInternal when generating signature for pubKey=$pubKey."
    )

    s
  }

  /** Verify a single signer partial signature with a vector of per-signer nonce
    * public pairs. This convenience overload selects the signer's nonce pair
    * using `signerIndex` from the provided `pubNonces` vector.
    */
  def partialSigVerify(
      partialSig: FieldElement,
      pubNonces: Vector[MuSigNoncePub],
      keySet: KeySet,
      message: ByteVector,
      signerIndex: Int): Boolean = {
    require(signerIndex >= 0 && signerIndex < keySet.length,
            s"Invalid signer index $signerIndex for ${keySet.length} signers")

    partialSigVerify(partialSig,
                     pubNonces(signerIndex),
                     aggregateNonces(pubNonces),
                     keySet(signerIndex),
                     keySet,
                     message)
  }

  /** Verify a single partial signature for a signer given the signer's nonce
    * pair, the aggregated nonce, the signer's public key, the key-set and
    * message. This wraps to `partialSigVerifyInternal`.
    */
  def partialSigVerify(
      partialSig: FieldElement,
      noncePub: MuSigNoncePub,
      aggNoncePub: MuSigNoncePub,
      pubKey: ECPublicKey,
      keySet: KeySet,
      message: ByteVector): Boolean = {
    val ctx =
      MuSigSessionContext(aggNoncePub, keySet, message)
    partialSigVerifyInternal(partialSig, Vector(noncePub), pubKey, ctx)
  }

  /** Internal verification routine implementing the MuSig2 partial signature
    * verification equation per BIP-0327.
    *
    * The check performed is: G * s == RE + g' * (a * e * P) where:
    *   - s is the partial signature scalar (FieldElement)
    *   - RE is the (possibly parity-negated) aggregated nonce point
    *   - g' is the parity-multiplied group coefficient applied to the signer
    *     public key (matching the signer's adjusted private key)
    *   - a is the session key aggregation coefficient for the signer
    *   - e is the challenge scalar for the session
    *   - P is the signer's public key
    *
    * The implementation uses `nonceSum` to collapse the per-signer nonce pair
    * and applies parity adjustments to both R and the public-key multiplier to
    * match the sign() implementation.
    */
  def partialSigVerifyInternal(
      partialSig: FieldElement,
      noncePubs: Vector[MuSigNoncePub],
      pubKey: ECPublicKey,
      sessionCtx: MuSigSessionContext): Boolean = {
    val values = sessionCtx.getSessionValues
    val keySet = sessionCtx.keySet
    val b = values.b
    val aggNonce = aggregateNonces(noncePubs)
    val e = values.e
    val REPrime = aggNonce.sumToKey(b)
    val RE = values.R.toPublicKey.parity match {
      case EvenParity => REPrime
      case OddParity =>
        REPrime.negate
    }
    val expectedS = CryptoParams.getG.multiply(partialSig)

    val a = keySet.getSessionKeyAggCoeff(sessionCtx, pubKey)
    val g = ParityMultiplier.fromParity(values.Q.toPublicKey.parity)
    // Match the sign computation: use values.gacc.multiply(g) so parity combination
    // is consistent with how the adjusted private key is computed in sign()
    val gPrime = values.gacc.multiply(g)
    val inner: ECPublicKey = gPrime.modify(pubKey).multiply(a).multiply(e)
    val actualS = RE.add(inner)
    expectedS == actualS
  }

  /** Aggregate a set of signer partial signature scalars into a BIP340 Schnorr
    * signature. This performs the final adjustments required by MuSig2 where
    * the sum of partial s values is further adjusted by the aggregated tweak
    * accumulator `tacc` multiplied by the session challenge e (and parity
    * multiplier g).
    */
  def partialSigAgg(
      sVals: Vector[FieldElement],
      aggNoncePub: MuSigNoncePub,
      keySet: KeySet,
      message: ByteVector): SchnorrDigitalSignature = {
    val ctx = MuSigSessionContext(aggNoncePub, keySet, message)

    partialSigAgg(sVals, ctx)
  }

  /** Internal aggregation helper which computes the final Schnorr signature
    * from individual s values and session context.
    */
  def partialSigAgg(
      sVals: Vector[FieldElement],
      ctx: MuSigSessionContext): SchnorrDigitalSignature = {
    val values = ctx.getSessionValues
    val e = values.e
    val aggPubNonce = values.R.toPublicKey
    val sSum = sVals.reduce(_.add(_))
    val g = ParityMultiplier.fromParity(values.Q.toPublicKey.parity)
    val s = sSum.add(
      g
        .modify(values.tacc)
        .multiply(e))

    SchnorrDigitalSignature(aggPubNonce.schnorrNonce, s, hashTypeOpt = None)
  }

  /** Deterministic signing helper that produces a deterministic nonce pair and
    * corresponding partial signature following the reference deterministic
    * construction in the BIP. This is useful for reproducible tests or for
    * deterministic wallets.
    *
    * Returns the public nonce pair and the scalar partial signature.
    */
  def deterministicSign(
      secretKey: ECPrivateKey,
      aggOtherNonce: MuSigNoncePub,
      keySet: KeySet,
      message: ByteVector,
      auxRandOpt: Option[ByteVector]): (MuSigNoncePub, FieldElement) = {
    require(
      auxRandOpt.forall(_.length == 32),
      s"auxRand must be 32 bytes if provided, got ${auxRandOpt.map(_.length)}")
    val secretKeyPrime = auxRandOpt match {
      case Some(auxRand) =>
        val auxHash = MuSigUtil.auxHash(auxRand)
        secretKey.bytes.xor(auxHash)
      case None => secretKey.bytes
    }
    val aggPubKey = keySet.aggPubKey
    val secrets: Vector[FieldElement] = 0
      .to(1)
      .map { i =>
        val bytes = secretKeyPrime ++ aggOtherNonce.bytes ++
          aggPubKey.toXOnly.bytes ++
          ByteVector.fromLong(message.length, size = 8) ++
          message ++
          ByteVector.fromByte(i.toByte)
        muSigDeterministicNonceHash(bytes)
      }
      .toVector
      .map(FieldElement.fromBytes)

    require(!secrets.contains(FieldElement.zero),
            "Derived nonce secrets cannot be zero")
    val r1: ECPublicKey = CryptoParams.getG.multiply(secrets.head)
    val r2: ECPublicKey = CryptoParams.getG.multiply(secrets(1))
    require(CryptoUtil.decodePoint(r1) != SecpPointInfinity)
    require(CryptoUtil.decodePoint(r2) != SecpPointInfinity)
    val pubNonce = MuSigNoncePub(r1.bytes ++ r2.bytes)

    val secNonce = MuSigNoncePriv(secrets.head.toPrivateKey,
                                  secrets(1).toPrivateKey,
                                  secretKey.publicKey)
    val aggNonce = aggregateNonces(Vector(pubNonce, aggOtherNonce))
    val ctx = MuSigSessionContext(aggNonce, keySet, message)
    (pubNonce, sign(secNonce, secretKey, ctx))
  }

  /** Aggregate per-signer nonce pairs into a single aggregated nonce pair by
    * point-wise addition. The returned `MuSigNoncePub` contains the summed r1
    * and r2 values respectively.
    */
  def aggregateNonces(nonces: Vector[MuSigNoncePub]): MuSigNoncePub = {
    val aggNonceKeys = 0.until(MuSigUtil.nonceNum).toVector.map { i =>
      nonces.zipWithIndex
        .map { case (multiNonce, idx) =>
          val nonce = if (i == 0) multiNonce.r1 else multiNonce.r2
          require(
            nonce != SecpPointInfinity,
            s"Nonce $i of signer ${idx + 1} of a nonce pair cannot be the point at infinity")
          nonce
        }
        .reduce(_.add(_))
    }
    MuSigNoncePub(aggNonceKeys(0), aggNonceKeys(1))
  }
}
