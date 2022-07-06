package org.bitcoins.crypto.musig

import org.bitcoins.crypto.{
  CryptoUtil,
  ECPublicKey,
  FieldElement,
  SchnorrPublicKey
}
import scodec.bits.ByteVector

/** The data relevant to computing and verifying MuSig partial signatures */
case class SigningSession(
    b: FieldElement,
    aggNonce: ECPublicKey,
    e: FieldElement)

object SigningSession {

  def computeB(
      aggNoncePub: MuSigNoncePub,
      keySet: KeySet,
      message: ByteVector): FieldElement = {
    val aggPubKey = keySet.aggPubKey.schnorrPublicKey

    val bHash =
      MuSigUtil.nonCoefHash(aggNoncePub.bytes ++ aggPubKey.bytes ++ message)

    FieldElement(new java.math.BigInteger(1, bHash.toArray))
  }

  def computeE(
      aggPubKey: SchnorrPublicKey,
      aggNonce: ECPublicKey,
      message: ByteVector): FieldElement = {
    val eBytes = CryptoUtil
      .sha256SchnorrChallenge(
        aggNonce.schnorrNonce.bytes ++ aggPubKey.bytes ++ message)
      .bytes

    FieldElement(new java.math.BigInteger(1, eBytes.toArray))
  }

  def getSessionValues(
      aggNoncePub: MuSigNoncePub,
      keySet: KeySet,
      message: ByteVector): SigningSession = {
    val aggPubKey = keySet.aggPubKey.schnorrPublicKey
    val b = computeB(aggNoncePub, keySet, message)
    val aggNonce = aggNoncePub.sumToKey(b)
    val e = computeE(aggPubKey, aggNonce, message)

    SigningSession(b, aggNonce, e)
  }

  def apply(
      aggNoncePub: MuSigNoncePub,
      keySet: KeySet,
      message: ByteVector): SigningSession = {
    getSessionValues(aggNoncePub, keySet, message)
  }
}
