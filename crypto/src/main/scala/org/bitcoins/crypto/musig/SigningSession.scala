package org.bitcoins.crypto.musig

import org.bitcoins.crypto.{
  CryptoUtil,
  ECPublicKey,
  FieldElement,
  SchnorrPublicKey
}
import scodec.bits.ByteVector

case class SigningSession(
    b: FieldElement,
    aggNonce: ECPublicKey,
    e: FieldElement)

object SigningSession {

  def computeB(
      aggMuSigNoncePub: MuSigNoncePub,
      keySet: KeySet,
      message: ByteVector): FieldElement = {
    val aggPubKey = keySet.aggPubKey.schnorrPublicKey

    val bHash = MuSig2Util.nonCoefHash(
      aggMuSigNoncePub.bytes ++ aggPubKey.bytes ++ message)

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
      aggMuSigNoncePub: MuSigNoncePub,
      keySet: KeySet,
      message: ByteVector): SigningSession = {
    val aggPubKey = keySet.aggPubKey.schnorrPublicKey
    val b = computeB(aggMuSigNoncePub, keySet, message)
    val aggNonce = aggMuSigNoncePub.sumToKey(b)
    val e = computeE(aggPubKey, aggNonce, message)

    SigningSession(b, aggNonce, e)
  }

  def apply(
      aggMuSigNoncePub: MuSigNoncePub,
      keySet: KeySet,
      message: ByteVector): SigningSession = {
    getSessionValues(aggMuSigNoncePub, keySet, message)
  }
}
