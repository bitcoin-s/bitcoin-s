package org.bitcoins.crypto.musig

import org.bitcoins.crypto._
import scodec.bits.ByteVector

// TODO test against secp256k1-zkp
// TODO refactor for niceness
// TODO easy optimizations (e.g. remove parity multiplications)
// TODO scaladocs and require messages
object MuSig2Util {

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

  def sign(
      noncePriv: MuSigNoncePriv,
      aggMuSigNoncePub: MuSigNoncePub,
      privKey: ECPrivateKey,
      message: ByteVector,
      keySet: KeySet): (ECPublicKey, FieldElement) = {
    val pubKey = privKey.publicKey
    val coef = keySet.keyAggCoef(pubKey.schnorrPublicKey)
    val SigningSession(b, aggNonce, e) =
      SigningSession(aggMuSigNoncePub, keySet, message)

    val adjustedNoncePriv = aggNonce.parity match {
      case EvenParity => noncePriv
      case OddParity  => noncePriv.negate
    }

    val gp = pubKey.parity match {
      case EvenParity => FieldElement.one
      case OddParity  => FieldElement.orderMinusOne
    }

    val g = keySet.aggPubKey.parity match {
      case EvenParity => FieldElement.one
      case OddParity  => FieldElement.orderMinusOne
    }

    val adjustedPrivKey = privKey.fieldElement
      .multiply(gp)
      .multiply(g)
      .multiply(keySet.tweakContext.parityAcc)

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
      MuSigNoncePub: MuSigNoncePub,
      aggMuSigNoncePub: MuSigNoncePub,
      pubKey: SchnorrPublicKey,
      keySet: KeySet,
      message: ByteVector): Boolean = {
    val SigningSession(b, aggNonce, e) =
      SigningSession(aggMuSigNoncePub, keySet, message)

    partialSigVerify(partialSig, MuSigNoncePub, pubKey, keySet, b, aggNonce, e)
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
      case OddParity  => nonceSum.multiply(FieldElement.orderMinusOne)
    }

    val g = keySet.aggPubKey.parity match {
      case EvenParity => FieldElement.one
      case OddParity  => FieldElement.orderMinusOne
    }

    val aggKeyParity = g.multiply(keySet.tweakContext.parityAcc) match {
      case FieldElement.one           => EvenParity
      case FieldElement.orderMinusOne => OddParity
      case _: FieldElement => // TODO TweakContext.parityAcc needs an ADT
        throw new RuntimeException("Something has gone very wrong.")
    }

    val aggKey = pubKey.toXOnly.publicKey(aggKeyParity)
    val a = keySet.keyAggCoef(pubKey)
    partialSig.getPublicKey == nonceSumAdjusted.add(
      aggKey.multiply(e.multiply(a)))
  }

  def signAgg(
      sVals: Vector[FieldElement],
      aggMuSigNoncePub: MuSigNoncePub,
      keySet: KeySet,
      message: ByteVector): SchnorrDigitalSignature = {
    val SigningSession(_, aggPubNonce, e) =
      SigningSession(aggMuSigNoncePub, keySet, message)
    val tweakData =
      MuSigTweakData(keySet.tweakContext, keySet.aggPubKey.parity, e)

    signAgg(sVals, aggPubNonce, Some(tweakData))
  }

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
