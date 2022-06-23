package org.bitcoins.crypto

import scodec.bits.ByteVector

object MuSig2Util {

  private val nonceNum: Int = 2 // 4

  type MultiNoncePriv = Vector[ECPrivateKey]
  type MultiNoncePub = Vector[ECPublicKey]
  type KeySet = Vector[ECPublicKey]

  def keySetSerialize(keySet: KeySet): ByteVector = {
    keySet.map(_.schnorrPublicKey.bytes).reduce(_ ++ _)
  }

  def aggHash(bytes: ByteVector): ByteVector = {
    CryptoUtil.taggedSha256(bytes, "MuSig2/agg").bytes
  }

  def nonHash(bytes: ByteVector): ByteVector = {
    CryptoUtil.taggedSha256(bytes, "MuSig2/non").bytes
  }

  def sigHash(bytes: ByteVector): ByteVector = {
    CryptoUtil.taggedSha256(bytes, "MuSig2/sig").bytes
  }

  def keyAggCoef(keySet: KeySet, key: ECPublicKey): FieldElement = {
    val bytes = aggHash(keySetSerialize(keySet) ++ key.schnorrPublicKey.bytes)

    FieldElement(bytes)
  }

  def keyAgg(keySet: KeySet): ECPublicKey = {
    keySet
      .map { key =>
        val coef = keyAggCoef(keySet, key)
        key.multiply(coef)
      }
      .reduce(_.add(_))
  }

  def genMultiNonce(): (MultiNoncePub, MultiNoncePriv) = {
    val privNonces = Vector.fill(nonceNum)(ECPrivateKey.freshPrivateKey)
    val pubNonces = privNonces.map(_.publicKey)

    (pubNonces, privNonces)
  }

  def aggNonces(nonces: Vector[MultiNoncePub]): MultiNoncePub = {
    0.until(nonceNum).toVector.map { j =>
      nonces.map(multiNonce => multiNonce(j)).reduce(_.add(_))
    }
  }

  def sign(
      noncePriv: MultiNoncePriv,
      aggMultiNoncePub: MultiNoncePub,
      privKey: ECPrivateKey,
      message: ByteVector,
      keySet: KeySet): (ECPublicKey, FieldElement) = {
    val pubKey = privKey.publicKey
    val coef = keyAggCoef(keySet, pubKey)
    val aggPubKey = keyAgg(keySet)
    val bBytes = nonHash(
      aggPubKey.schnorrPublicKey.bytes ++ aggMultiNoncePub
        .map(_.schnorrPublicKey.bytes)
        .reduce(_ ++ _) ++ message)
    val b = FieldElement(bBytes)
    val aggNonce = aggMultiNoncePub.tail
      .foldLeft((FieldElement.one, aggMultiNoncePub.head)) {
        case ((prevPow, sumSoFar), nonce) =>
          val pow = prevPow.multiply(b)
          val point = nonce.multiply(pow)

          (pow, sumSoFar.add(point))
      }
      ._2

    val cBytes = sigHash(
      aggPubKey.schnorrPublicKey.bytes ++ aggNonce.schnorrNonce.bytes ++ message)
    val c = FieldElement(cBytes)

    val privNonceSum = noncePriv
      .foldLeft((FieldElement.one, FieldElement.zero)) {
        case ((pow, sumSoFar), privNonce) =>
          val scalar = privNonce.fieldElement.multiply(pow)
          val nextPow = pow.multiply(b)

          (nextPow, sumSoFar.add(scalar))
      }
      ._2

    val s = privKey.fieldElement.multiply(c).multiply(coef).add(privNonceSum)

    (aggNonce, s)
  }

  def signAgg(
      sVals: Vector[FieldElement],
      aggPubNonce: ECPublicKey): SchnorrDigitalSignature = {
    val s = sVals.reduce(_.add(_))

    SchnorrDigitalSignature(aggPubNonce.schnorrNonce, s)
  }

  def verify(
      aggPubKey: ECPublicKey,
      message: ByteVector,
      sig: SchnorrDigitalSignature): Boolean = {
    val aggPubNonce = sig.rx
    val s = sig.sig
    val cBytes = sigHash(
      aggPubKey.schnorrPublicKey.bytes ++ aggPubNonce.bytes ++ message)
    val c = FieldElement(cBytes)

    s.getPublicKey == aggPubNonce.publicKey.add(aggPubKey.multiply(c))
  }
}
