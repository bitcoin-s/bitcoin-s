package org.bitcoins.crypto.musig

import org.bitcoins.crypto.*
import scodec.bits.ByteVector

// TODO test against secp256k1-zkp someday
/** Contains constants, hash functions, and signing/verification functionality
  * for MuSig
  */
object MuSigUtil {

  val nonceNum: Int = 2

  def aggListHash(bytes: ByteVector): ByteVector = {
    CryptoUtil.taggedSha256(bytes, "KeyAgg list").bytes
  }

  def aggCoefHash(bytes: ByteVector): ByteVector = {
    CryptoUtil.taggedSha256(bytes, "KeyAgg coefficient").bytes
  }

  def nonHash(bytes: ByteVector): ByteVector = {
    CryptoUtil.taggedSha256(bytes, "MuSig/nonce").bytes
  }

  def nonCoefHash(bytes: ByteVector): ByteVector = {
    CryptoUtil.taggedSha256(bytes, "MuSig/noncecoef").bytes
  }

  def auxHash(bytes: ByteVector): ByteVector = {
    CryptoUtil.taggedSha256(bytes, "MuSig/aux").bytes
  }

  def muSigDeterministicNonceHash(bytes: ByteVector): ByteVector = {
    CryptoUtil.taggedSha256(bytes, "MuSig/deterministic/nonce").bytes
  }

  /** nonces(0) + nonces(1)*b + nonces(2)*b*b + ... */
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

  /** Generates a MuSigNoncePriv given 32 bytes of entropy from preRand, and
    * possibly some other sources, as specified in the BIP.
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

  /** Generates 32 bytes of entropy and constructs a MuSigNoncePriv from this,
    * and possibly some other sources, as specified in the BIP.
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

  /** Generates a MuSig partial signature, accompanied by the aggregate R value
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

  def sign(
      noncePriv: MuSigNoncePriv,
      privKey: ECPrivateKey,
      signingSession: MuSigSessionContext): FieldElement = {
    val pubKey = privKey.publicKey
    require(
      pubKey == noncePriv.publicKey,
      s"Nonce private key must be derived from the same public key, got ${pubKey} and ${noncePriv.publicKey}")
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

  /** Aggregates MuSig partial signatures into a BIP340 SchnorrDigitalSignature
    */
  def signAgg(
      sVals: Vector[FieldElement],
      aggNoncePub: MuSigNoncePub,
      keySet: KeySet,
      message: ByteVector): SchnorrDigitalSignature = {
    val ctx = MuSigSessionContext(aggNoncePub, keySet, message)

    partialSigAgg(sVals, ctx)
  }

  /** Aggregates MuSig partial signatures into a BIP340 SchnorrDigitalSignature
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

  /** Sums the given nonces and returns the aggregate MuSigNoncePub */
  def aggregateNonces(nonces: Vector[MuSigNoncePub]): MuSigNoncePub = {
    val aggNonceKeys = 0.until(MuSigUtil.nonceNum).toVector.map { i =>
      nonces.zipWithIndex
        .map { case (multiNonce, idx) =>
          val nonce = multiNonce(i)
          require(
            nonce != SecpPointInfinity,
            s"Nonce $i of signer ${idx + 1} is the point at infinity, which is not allowed in MuSig2")
          nonce
        }
        .reduce(_.add(_))
    }
    MuSigNoncePub(aggNonceKeys)
  }
}
