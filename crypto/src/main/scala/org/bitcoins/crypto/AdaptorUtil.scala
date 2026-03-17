package org.bitcoins.crypto

import scodec.bits.ByteVector

/** Implements the ECDSA Adaptor Signing Specification:
  * https://github.com/discreetlogcontracts/dlcspecs/blob/d01595b70269d4204b05510d19bba6a4f4fcff23/ECDSA-adaptor.md
  *
  * Note that the naming is not entirely consistent between the specification
  * and this file in hopes of making this code more readable.
  *
  * The naming in this file more closely matches the naming in the secp256k1-zkp
  * implementation:
  * https://github.com/ElementsProject/secp256k1-zkp/tree/master/src/modules/ecdsa_adaptor
  *
  * Legend: x <> privKey X <> pubKey y <> adaptorSecret Y <>
  * adaptorPoint/adaptor messageHash <> dataToSign/data/message R_a <>
  * untweakedNonce R <> tweakedNonce proof <> (e, s)
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

    val bytesToHash = maskedKey ++ adaptorPoint.bytes ++ message
    val nonceHash = algoName match {
      case "DLEQ"             => CryptoUtil.sha256DLEQ(bytesToHash)
      case "ECDSAadaptor/non" => CryptoUtil.sha256ECDSAAdaptorNonce(bytesToHash)
      case _: String          => CryptoUtil.taggedSha256(bytesToHash, algoName)
    }

    FieldElement(nonceHash.bytes)
  }

  /** Computes s_a = inverse(k) * (dataToSign + rx*privateKey) which is the
    * third from last step in
    * https://github.com/discreetlogcontracts/dlcspecs/blob/d01595b70269d4204b05510d19bba6a4f4fcff23/ECDSA-adaptor.md#encrypted-signing
    */
  private def adaptorSignHelper(
      dataToSign: ByteVector,
      k: FieldElement,
      r: ECPublicKey,
      privateKey: ECPrivateKey): FieldElement = {
    CryptoUtil.decodePoint(r) match {
      case SecpPointInfinity =>
        throw new IllegalArgumentException(
          s"Invalid point, got=$SecpPointInfinity")
      case point: SecpPointFinite =>
        val rx = FieldElement(point.x.toBigInteger)
        val x = privateKey.fieldElement
        val m = FieldElement(dataToSign)
        val kInv = k.inverse

        rx.multiply(x).add(m).multiply(kInv)
    }
  }

  /** Implements
    * https://github.com/discreetlogcontracts/dlcspecs/blob/d01595b70269d4204b05510d19bba6a4f4fcff23/ECDSA-adaptor.md#encrypted-signing
    */
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
    val tweakedNonce = adaptorPoint.multiply(k) // k*Y

    // DLEQ_prove((G,R'),(Y, R))
    val (proofE, proofS) = DLEQUtil.dleqProve(k, adaptorPoint, auxRand)

    // s' = k^-1*(m + rx*x)
    val adaptedSig = adaptorSignHelper(dataToSign, k, tweakedNonce, privateKey)

    ECAdaptorSignature(tweakedNonce, untweakedNonce, adaptedSig, proofE, proofS)
  }

  /** Computes R = inverse(s) * (msg*G + rx*pubKey) = inverse(s) * (msg +
    * rx*privKey) * G
    */
  private def adaptorVerifyHelper(
      rx: FieldElement,
      s: FieldElement,
      pubKey: ECPublicKey,
      msg: ByteVector): FieldElement = {
    val m = FieldElement(msg)
    val untweakedPoint =
      m.getPublicKey.add(pubKey.multiply(rx)).multiply(s.inverse)

    FieldElement(untweakedPoint.bytes.tail)
  }

  /** https://github.com/discreetlogcontracts/dlcspecs/blob/d01595b70269d4204b05510d19bba6a4f4fcff23/ECDSA-adaptor.md#encryption-verification
    */
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
      val tweakedNoncex = FieldElement(
        CurveCoordinate(adaptorSig.tweakedNonce.bytes.tail).toBigInteger)
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

  /** Implements
    * https://github.com/discreetlogcontracts/dlcspecs/blob/d01595b70269d4204b05510d19bba6a4f4fcff23/ECDSA-adaptor.md#decryption
    */
  def adaptorComplete(
      adaptorSecret: ECPrivateKey,
      adaptorSig: ECAdaptorSignature): ECDigitalSignature = {
    val rx = FieldElement(adaptorSig.tweakedNonce.bytes.tail)
    val correctedS = adaptorSig.adaptedS.multInv(adaptorSecret.fieldElement)

    val sig = ECDigitalSignature.fromRS(BigInt(rx.toBigInteger),
                                        BigInt(correctedS.toBigInteger))
    DERSignatureUtil.lowS(sig)
  }

  /** Implements
    * https://github.com/discreetlogcontracts/dlcspecs/blob/d01595b70269d4204b05510d19bba6a4f4fcff23/ECDSA-adaptor.md#key-recovery
    */
  def extractAdaptorSecret(
      sig: ECDigitalSignature,
      adaptorSig: ECAdaptorSignature,
      adaptor: ECPublicKey): ECPrivateKey = {
    require(adaptorSig.tweakedNonce.bytes.tail == sig.rBytes,
            "Adaptor signature must be related to signature")

    val secretOrNeg = adaptorSig.adaptedS.multInv(FieldElement(sig.s))

    require(secretOrNeg.getPublicKey.bytes.tail == adaptor.bytes.tail,
            s"Invalid inputs: $sig, $adaptorSig, and $adaptor")

    if (secretOrNeg.getPublicKey == adaptor) {
      secretOrNeg.toPrivateKey
    } else {
      secretOrNeg.negate.toPrivateKey
    }
  }

  /** https://github.com/ZhePang/Python_Specification_for_Schnorr_Adaptor/blob/51aa10bd6785d22d8fe4de85a4ecd2200efe1ef3/reference.py#L162
    * @param privateKey
    * @param adaptorPoint
    * @param dataToSign
    * @param auxRand
    * @return
    */
  def schnorrAdaptorSign(
      privateKey: ECPrivateKey,
      adaptorPoint: ECPublicKey,
      dataToSign: ByteVector,
      auxRand: Option[ByteVector]): SchnorrAdaptorSignature = {
    val pubKey = privateKey.publicKey
    val d = pubKey.parity match {
      case EvenParity => privateKey.fieldElement
      case OddParity  => privateKey.fieldElement.negate
    }
    val t = d.bytes.xor(
      CryptoUtil
        .sha256SchnorrAdaptorAux(auxRand.getOrElse(ByteVector.empty))
        .bytes)
    val k0 = FieldElement.fromBytes(
      CryptoUtil
        .sha256SchnorrAdaptorNonce(
          t ++ adaptorPoint.bytes ++ pubKey.toXOnly.bytes ++ dataToSign)
        .bytes)
    require(!k0.isZero, s"Cannot have zero nonce")
    val R = CryptoParams.getG.multiply(k0)

    val R0: SecpPointFinite = R.toPoint.add(adaptorPoint.toPoint) match {
      case f: SecpPointFinite => f
      case SecpPointInfinity =>
        throw new IllegalArgumentException(
          s"Cannot have point at infinity for nonce")
    }

    val k = R0.toPublicKey.parity match {
      case EvenParity => k0
      case OddParity  => k0.negate
    }
    val e = FieldElement.fromBytes(
      CryptoUtil
        .sha256SchnorrChallenge(
          R0.toPublicKey.toXOnly.bytes ++ pubKey.toXOnly.bytes ++ dataToSign)
        .bytes)
    val s = k.add(e.multiply(d))
    val adaptorSig = SchnorrAdaptorSignature(R0.toPublicKey, s)
    require(schnorrAdaptorVerify(adaptorSig,
                                 pubKey.toXOnly,
                                 dataToSign,
                                 adaptorPoint),
            s"Adaptor signature did not verify, got $adaptorSig")
    adaptorSig
  }

  def schnorrAdaptorVerify(
      adaptorSig: SchnorrAdaptorSignature,
      pubKey: XOnlyPubKey,
      data: ByteVector,
      adaptor: ECPublicKey): Boolean = {
    val adaptorExpected = schnorrExtractAdaptor(data, pubKey, adaptorSig)
    adaptorExpected == adaptor
  }

  def schnorrExtractAdaptor(
      data: ByteVector,
      pubKey: XOnlyPubKey,
      adaptorSig: SchnorrAdaptorSignature): ECPublicKey = {
    val P = pubKey.publicKey.toPoint
    val s0 = adaptorSig.s
    val R0 = adaptorSig.R.toPoint
    val e = FieldElement.fromBytes(
      CryptoUtil
        .sha256SchnorrChallenge(
          R0.toPublicKey.toXOnly.bytes ++ pubKey.bytes ++ data)
        .bytes)
    val eNegate = FieldElement(CryptoParams.getN.subtract(e.toBigInteger))
    val R =
      P.multiply(eNegate).add(CryptoParams.getG.multiply(s0).toPoint) match {
        case f: SecpPointFinite => f
        case SecpPointInfinity =>
          throw new IllegalArgumentException(
            s"Cannot have point at infinity for nonce")
      }
    val T = R0.toPublicKey.parity match {
      case EvenParity => R0.add(R.negate)
      case OddParity  => R0.add(R)
    }
    T match {
      case f: SecpPointFinite => f.toPublicKey
      case SecpPointInfinity =>
        throw new IllegalArgumentException(
          s"Cannot have point at infinity for adaptor point")
    }
  }

  def schnorrAdaptorComplete(
      adaptorSecret: ECPrivateKey,
      adaptorSig: SchnorrAdaptorSignature): SchnorrDigitalSignature = {
    val s = adaptorSig.s
    val t = adaptorSecret.fieldElement
    val sPrime = adaptorSig.R.parity match {
      case EvenParity => s.add(t)
      case OddParity  => s.subtract(t)
    }

    val rXOnly = adaptorSig.R.toXOnly
    val nonce = SchnorrNonce(rXOnly.bytes)
    SchnorrDigitalSignature(nonce, sPrime, None)
  }

  def schnorrExtractSecret(
      sig: SchnorrDigitalSignature,
      adaptorSig: SchnorrAdaptorSignature,
      adaptor: ECPublicKey): ECPrivateKey = {
    ???
  }

}
