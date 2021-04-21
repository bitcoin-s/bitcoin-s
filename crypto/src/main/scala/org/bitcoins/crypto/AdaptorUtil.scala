package org.bitcoins.crypto

import scodec.bits.ByteVector

/** Implements the ECDSA Adaptor Signing Specification:
  * https://github.com/discreetlogcontracts/dlcspecs/blob/d01595b70269d4204b05510d19bba6a4f4fcff23/ECDSA-adaptor.md
  *
  * Note that the naming is not entirely consistent between the specification
  * and this file in hopes of making this code more readable.
  *
  * The naming in this file more closely matches the naming in the secp256k1-zkp implementation:
  * https://github.com/ElementsProject/secp256k1-zkp/tree/master/src/modules/ecdsa_adaptor
  *
  * Legend:
  * x <> privKey
  * X <> pubKey
  * y <> adaptorSecret
  * Y <> adaptorPoint/adaptor
  * messageHash <> dataToSign/data/message
  * R_a <> untweakedNonce
  * R <> tweakedNonce
  * proof <> (e, s)
  */
object AdaptorUtil {

  /** Generates a secure random nonce as is done in BIP340:
    * https://github.com/bitcoin/bips/blob/master/bip-0340.mediawiki#default-signing
    */
  def adaptorNonce(
      message: ByteVector,
      privKey: ECPrivateKey,
      adaptorPoint: ECPublicKey,
      algoName: String,
      auxRand: ByteVector): FieldElement = {
    val randHash = CryptoUtil.sha256ECDSAAdaptorAux(auxRand).bytes
    val maskedKey = randHash.xor(privKey.bytes)

    val bytesToHash = maskedKey ++ adaptorPoint.compressed.bytes ++ message
    val nonceHash = algoName match {
      case "DLEQ"             => CryptoUtil.sha256DLEQ(bytesToHash)
      case "ECDSAadaptor/non" => CryptoUtil.sha256ECDSAAdaptorNonce(bytesToHash)
      case _: String          => CryptoUtil.taggedSha256(bytesToHash, algoName)
    }

    FieldElement(nonceHash.bytes)
  }

  /** Computes s_a = inverse(k) * (dataToSign + rx*privateKey)
    * which is the third from last step in
    * https://github.com/discreetlogcontracts/dlcspecs/blob/d01595b70269d4204b05510d19bba6a4f4fcff23/ECDSA-adaptor.md#encrypted-signing
    */
  private def adaptorSignHelper(
      dataToSign: ByteVector,
      k: FieldElement,
      r: ECPublicKey,
      privateKey: ECPrivateKey): FieldElement = {
    CryptoUtil.decodePoint(r) match {
      case ECPointInfinity =>
        throw new IllegalArgumentException(
          s"Invalid point, got=$ECPointInfinity")
      case point: ECPointImpl =>
        val rx = FieldElement(point.x.toBigInteger)
        val x = privateKey.fieldElement
        val m = FieldElement(dataToSign)
        val kInv = k.inverse

        rx.multiply(x).add(m).multiply(kInv)
    }
  }

  /** Implements https://github.com/discreetlogcontracts/dlcspecs/blob/d01595b70269d4204b05510d19bba6a4f4fcff23/ECDSA-adaptor.md#encrypted-signing */
  def adaptorSign(
      privateKey: ECPrivateKey,
      adaptorPoint: ECPublicKey,
      dataToSign: ByteVector,
      auxRand: ByteVector): ECAdaptorSignature = {
    val k = adaptorNonce(dataToSign,
                         privateKey,
                         adaptorPoint,
                         "ECDSAadaptor/non",
                         auxRand)

    if (k.isZero) {
      throw new RuntimeException("Nonce cannot be zero.")
    }

    val untweakedNonce = k.getPublicKey // k*G
    val tweakedNonce = adaptorPoint.tweakMultiply(k) // k*Y

    // DLEQ_prove((G,R'),(Y, R))
    val (proofE, proofS) = DLEQUtil.dleqProve(k, adaptorPoint, auxRand)

    // s' = k^-1*(m + rx*x)
    val adaptedSig = adaptorSignHelper(dataToSign, k, tweakedNonce, privateKey)

    ECAdaptorSignature(tweakedNonce, untweakedNonce, adaptedSig, proofE, proofS)
  }

  /** Computes R = inverse(s) * (msg*G + rx*pubKey) = inverse(s) * (msg + rx*privKey) * G */
  private def adaptorVerifyHelper(
      rx: FieldElement,
      s: FieldElement,
      pubKey: ECPublicKey,
      msg: ByteVector): FieldElement = {
    val m = FieldElement(msg)
    val untweakedPoint =
      m.getPublicKey.add(pubKey.tweakMultiply(rx)).tweakMultiply(s.inverse)

    FieldElement(untweakedPoint.compressed.bytes.tail)
  }

  /** https://github.com/discreetlogcontracts/dlcspecs/blob/d01595b70269d4204b05510d19bba6a4f4fcff23/ECDSA-adaptor.md#encryption-verification */
  def adaptorVerify(
      adaptorSig: ECAdaptorSignature,
      pubKey: ECPublicKey,
      data: ByteVector,
      adaptor: ECPublicKey): Boolean = {
    val validProof = DLEQUtil.dleqVerify(
      adaptorSig.dleqProofS,
      adaptorSig.dleqProofE,
      adaptorSig.untweakedNonce,
      adaptor,
      adaptorSig.tweakedNonce
    )

    if (validProof) {
      val tweakedNoncex = FieldElement(adaptorSig.tweakedNonce.bytes.tail)
      val untweakedNoncex = FieldElement(adaptorSig.untweakedNonce.bytes.tail)

      if (tweakedNoncex.isZero || untweakedNoncex.isZero) {
        false
      } else {

        val untweakedRx =
          adaptorVerifyHelper(tweakedNoncex, adaptorSig.adaptedS, pubKey, data)

        untweakedRx == untweakedNoncex
      }
    } else {
      false
    }
  }

  /** Implements https://github.com/discreetlogcontracts/dlcspecs/blob/d01595b70269d4204b05510d19bba6a4f4fcff23/ECDSA-adaptor.md#decryption */
  def adaptorComplete(
      adaptorSecret: ECPrivateKey,
      adaptorSig: ECAdaptorSignature): ECDigitalSignature = {
    val rx = FieldElement(adaptorSig.tweakedNonce.bytes.tail)
    val correctedS = adaptorSig.adaptedS.multInv(adaptorSecret.fieldElement)

    val sig = ECDigitalSignature.fromRS(BigInt(rx.toBigInteger),
                                        BigInt(correctedS.toBigInteger))
    DERSignatureUtil.lowS(sig)
  }

  /** Implements https://github.com/discreetlogcontracts/dlcspecs/blob/d01595b70269d4204b05510d19bba6a4f4fcff23/ECDSA-adaptor.md#key-recovery */
  def extractAdaptorSecret(
      sig: ECDigitalSignature,
      adaptorSig: ECAdaptorSignature,
      adaptor: ECPublicKey): ECPrivateKey = {
    require(adaptorSig.tweakedNonce.bytes.tail == sig.rBytes,
            "Adaptor signature must be related to signature")

    val secretOrNeg = adaptorSig.adaptedS.multInv(FieldElement(sig.s))

    require(
      secretOrNeg.getPublicKey.compressed.bytes.tail == adaptor.compressed.bytes.tail,
      s"Invalid inputs: $sig, $adaptorSig, and $adaptor")

    if (secretOrNeg.getPublicKey == adaptor) {
      secretOrNeg.toPrivateKey
    } else {
      secretOrNeg.negate.toPrivateKey
    }
  }

}
