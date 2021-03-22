package org.bitcoins.crypto

import scodec.bits.ByteVector

object AdaptorUtil {
  import ECAdaptorSignature.{deserializePoint, serializePoint}

  // Compute s' = k^-1 * (dataToSign + rx*privateKey)
  private def adaptorSignHelper(
      dataToSign: ByteVector,
      k: FieldElement,
      r: ECPublicKey,
      privateKey: ECPrivateKey): FieldElement = {
    CryptoUtil.decodePoint(r) match {
      case ECPointInfinity =>
        throw new IllegalArgumentException(
          s"Invalid point, got=${ECPointInfinity}")
      case point: ECPointImpl =>
        val rx = FieldElement(point.x.toBigInteger)
        val x = privateKey.fieldElement
        val m = FieldElement(dataToSign)
        val kInv = k.inverse

        rx.multiply(x).add(m).multiply(kInv)
    }
  }

  def adaptorSign(
      privateKey: ECPrivateKey,
      adaptorPoint: ECPublicKey,
      dataToSign: ByteVector): ECAdaptorSignature = {
    // Include dataToSign and adaptor in nonce derivation
    val hash =
      CryptoUtil.sha256(dataToSign ++ serializePoint(adaptorPoint))
    val k = DLEQUtil.dleqNonceFunc(hash.bytes,
                                   privateKey.fieldElement,
                                   "ECDSAAdaptorNon")

    if (k.isZero) {
      throw new RuntimeException("Nonce cannot be zero.")
    }

    val untweakedNonce = k.getPublicKey // k*G
    val tweakedNonce = adaptorPoint.tweakMultiply(k) // k*Y

    // DLEQ_prove((G,R'),(Y, R))
    val (proofS, proofE) =
      DLEQUtil.dleqProve(k, adaptorPoint, "ECDSAAdaptorSig")

    // s' = k^-1*(m + rx*x)
    val adaptedSig = adaptorSignHelper(dataToSign, k, tweakedNonce, privateKey)

    ECAdaptorSignature(tweakedNonce, adaptedSig, untweakedNonce, proofS, proofE)
  }

  // Compute R'x = s^-1 * (msg*G + rx*pubKey) = s^-1 * (msg + rx*privKey) * G
  private def adaptorVerifyHelper(
      rx: FieldElement,
      s: FieldElement,
      pubKey: ECPublicKey,
      msg: ByteVector): FieldElement = {
    val m = FieldElement(msg)
    val untweakedPoint =
      m.getPublicKey.add(pubKey.tweakMultiply(rx)).tweakMultiply(s.inverse)

    FieldElement(untweakedPoint.bytes.tail)
  }

  def adaptorVerify(
      adaptorSig: ECAdaptorSignature,
      pubKey: ECPublicKey,
      data: ByteVector,
      adaptor: ECPublicKey): Boolean = {
    val untweakedNonce = deserializePoint(adaptorSig.dleqProof.take(33))
    val proofS = FieldElement(adaptorSig.dleqProof.drop(33).take(32))
    val proofR = FieldElement(adaptorSig.dleqProof.drop(65))

    val tweakedNonce = deserializePoint(adaptorSig.adaptedSig.take(33))
    val adaptedSig = FieldElement(adaptorSig.adaptedSig.drop(33))

    val validProof = DLEQUtil.dleqVerify(
      "ECDSAAdaptorSig",
      proofS,
      proofR,
      untweakedNonce,
      adaptor,
      tweakedNonce
    )

    if (validProof) {
      val tweakedNoncex = FieldElement(tweakedNonce.bytes.tail)
      val untweakedNoncex = FieldElement(untweakedNonce.bytes.tail)

      if (tweakedNoncex.isZero || untweakedNoncex.isZero) {
        false
      } else {

        val untweakedRx =
          adaptorVerifyHelper(tweakedNoncex, adaptedSig, pubKey, data)

        untweakedRx == untweakedNoncex
      }
    } else {
      false
    }
  }

  def adaptorComplete(
      adaptorSecret: ECPrivateKey,
      adaptedSig: ByteVector): ECDigitalSignature = {
    val tweakedNonce: ECPublicKey =
      ECAdaptorSignature.deserializePoint(adaptedSig.take(33))
    val rx = FieldElement(tweakedNonce.bytes.tail)
    val adaptedS: FieldElement = FieldElement(adaptedSig.drop(33))
    val correctedS = adaptedS.multInv(adaptorSecret.fieldElement)

    val sig = ECDigitalSignature.fromRS(BigInt(rx.toBigInteger),
                                        BigInt(correctedS.toBigInteger))
    DERSignatureUtil.lowS(sig)
  }

  def extractAdaptorSecret(
      sig: ECDigitalSignature,
      adaptorSig: ECAdaptorSignature,
      adaptor: ECPublicKey): ECPrivateKey = {
    val secretOrNeg = adaptorSig.adaptedS.multInv(FieldElement(sig.s))
    if (secretOrNeg.getPublicKey == adaptor) {
      secretOrNeg.toPrivateKey
    } else {
      secretOrNeg.negate.toPrivateKey
    }
  }

}
