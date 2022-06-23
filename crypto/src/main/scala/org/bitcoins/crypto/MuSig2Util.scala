package org.bitcoins.crypto

import scodec.bits.ByteVector

object MuSig2Util {

  private val nonceNum: Int = 2

  type MultiNoncePriv = Vector[ECPrivateKey]
  type MultiNoncePub = Vector[ECPublicKey]

  case class KeySet(keys: Vector[ECPublicKey]) {
    keys.init.zip(keys.tail).foreach { case (key1, key2) =>
      require(key1.hex.compareTo(key2.hex) <= 0,
              "Keys must be sorted lexicographically")
    }

    lazy val serialize: ByteVector = {
      keys.map(_.schnorrPublicKey.bytes).reduce(_ ++ _)
    }

    def keyAggCoef(key: ECPublicKey): FieldElement = {
      val bytes = aggHash(this.serialize ++ key.schnorrPublicKey.bytes)

      FieldElement(bytes)
    }

    lazy val aggPubKey: ECPublicKey = {
      keys
        .map { key =>
          val coef = keyAggCoef(key)
          key.multiply(coef)
        }
        .reduce(_.add(_))
    }
  }

  object KeySet {

    def apply(keys: Vector[ECPublicKey]): KeySet = {
      val sortedKeys = keys.sorted(Ordering.by[ECPublicKey, String](_.hex))
      new KeySet(sortedKeys)
    }

    def apply(keys: ECPublicKey*): KeySet = {
      KeySet(keys.toVector)
    }
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
    val coef = keySet.keyAggCoef(pubKey)
    val aggPubKey = keySet.aggPubKey
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
