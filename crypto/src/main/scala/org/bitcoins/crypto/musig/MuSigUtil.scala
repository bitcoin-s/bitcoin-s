package org.bitcoins.crypto.musig

import org.bitcoins.crypto._
import scodec.bits.ByteVector

// TODO test against secp256k1-zkp
// TODO scaladocs and require messages
/** Contains constants, hash functions, and signing/verification functionality for MuSig */
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

  /** Generates a MuSig partial signature, accompanied by the aggregate R value */
  def sign(
      noncePriv: MuSigNoncePriv,
      aggNoncePub: MuSigNoncePub,
      privKey: ECPrivateKey,
      message: ByteVector,
      keySet: KeySet): (ECPublicKey, FieldElement) = {
    val pubKey = privKey.publicKey
    val coef = keySet.keyAggCoef(pubKey.schnorrPublicKey)
    val SigningSession(b, aggNonce, e) =
      SigningSession(aggNoncePub, keySet, message)

    val adjustedNoncePriv = aggNonce.parity match {
      case EvenParity => noncePriv
      case OddParity  => noncePriv.negate
    }

    val gp = ParityMultiplier.fromParity(pubKey.parity)

    val g = ParityMultiplier.fromParity(keySet.aggPubKey.parity)

    val adjustedPrivKey = gp
      .multiply(g)
      .multiply(keySet.tweakContext.parityAcc)
      .modify(privKey.fieldElement)

    val privNonceSum = adjustedNoncePriv.sumToKey(b)

    val s = adjustedPrivKey.multiply(e).multiply(coef).add(privNonceSum)

    require(
      partialSigVerify(s,
                       noncePriv.toPublicNonces,
                       pubKey.schnorrPublicKey,
                       keySet,
                       b,
                       aggNonce,
                       e))

    (aggNonce, s)
  }

  def partialSigVerify(
      partialSig: FieldElement,
      pubNonces: Vector[MuSigNoncePub],
      keySet: KeySet,
      message: ByteVector,
      signerIndex: Int): Boolean = {
    require(signerIndex >= 0 && signerIndex < keySet.length)

    partialSigVerify(partialSig,
                     pubNonces(signerIndex),
                     MuSigNoncePub.aggregate(pubNonces),
                     keySet(signerIndex),
                     keySet,
                     message)
  }

  def partialSigVerify(
      partialSig: FieldElement,
      noncePub: MuSigNoncePub,
      aggNoncePub: MuSigNoncePub,
      pubKey: SchnorrPublicKey,
      keySet: KeySet,
      message: ByteVector): Boolean = {
    val SigningSession(b, aggNonce, e) =
      SigningSession(aggNoncePub, keySet, message)

    partialSigVerify(partialSig, noncePub, pubKey, keySet, b, aggNonce, e)
  }

  def partialSigVerify(
      partialSig: FieldElement,
      noncePub: MuSigNoncePub,
      pubKey: SchnorrPublicKey,
      keySet: KeySet,
      b: FieldElement,
      aggNonce: ECPublicKey,
      e: FieldElement): Boolean = {
    val nonceSum = noncePub.sumToKey(b)
    val nonceSumAdjusted = aggNonce.parity match {
      case EvenParity => nonceSum
      case OddParity  => nonceSum.negate
    }

    val g = ParityMultiplier.fromParity(keySet.aggPubKey.parity)
    val aggKeyParity = g.multiply(keySet.tweakContext.parityAcc).toParity

    val aggKey = pubKey.toXOnly.publicKey(aggKeyParity)
    val a = keySet.keyAggCoef(pubKey)
    partialSig.getPublicKey == nonceSumAdjusted.add(
      aggKey.multiply(e.multiply(a)))
  }

  /** Aggregates MuSig partial signatures into a BIP340 SchnorrDigitalSignature */
  def signAgg(
      sVals: Vector[FieldElement],
      aggNoncePub: MuSigNoncePub,
      keySet: KeySet,
      message: ByteVector): SchnorrDigitalSignature = {
    val SigningSession(_, aggNonce, e) =
      SigningSession(aggNoncePub, keySet, message)
    val tweakData =
      MuSigTweakData(keySet.tweakContext, keySet.aggPubKey.parity, e)

    signAgg(sVals, aggNonce, Some(tweakData))
  }

  /** Aggregates MuSig partial signatures into a BIP340 SchnorrDigitalSignature */
  def signAgg(
      sVals: Vector[FieldElement],
      aggPubNonce: ECPublicKey,
      tweakDataOpt: Option[MuSigTweakData] = None): SchnorrDigitalSignature = {
    val sSum = sVals.reduce(_.add(_))
    val s = tweakDataOpt match {
      case Some(tweakData) => sSum.add(tweakData.additiveTweak)
      case None            => sSum
    }

    SchnorrDigitalSignature(aggPubNonce.schnorrNonce, s)
  }
}
