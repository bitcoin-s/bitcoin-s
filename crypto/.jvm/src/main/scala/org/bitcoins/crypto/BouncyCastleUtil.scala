package org.bitcoins.crypto

import org.bouncycastle.crypto.digests.SHA256Digest
import org.bouncycastle.crypto.params.{
  ECPrivateKeyParameters,
  ECPublicKeyParameters
}
import org.bouncycastle.crypto.signers.{ECDSASigner, HMacDSAKCalculator}
import org.bouncycastle.math.ec.ECCurve
import scodec.bits.ByteVector

import java.math.BigInteger
import scala.util.Try

object BouncyCastleUtil {

  private val curve: ECCurve = BouncyCastleCryptoParams.curve.getCurve
  private val G = BouncyCastleCryptoParams.curve.getG

  private def getBigInteger(bytes: ByteVector): BigInteger = {
    new BigInteger(1, bytes.toArray)
  }

  def pubKeyTweakMul(publicKey: ECPublicKey, tweak: ByteVector): ECPublicKey = {
    val point = decodePoint(publicKey).multiply(getBigInteger(tweak))
    decodePubKey(point, publicKey.isCompressed)
  }

  def decodePoint(bytes: ByteVector): org.bouncycastle.math.ec.ECPoint = {
    curve.decodePoint(bytes.toArray)
  }

  def decodePoint(pubKey: ECPublicKey): org.bouncycastle.math.ec.ECPoint = {
    decodePoint(pubKey.bytes)
  }

  def decodePubKey(
      point: org.bouncycastle.math.ec.ECPoint,
      isCompressed: Boolean = true): ECPublicKey = {
    val bytes = point.getEncoded(isCompressed)
    ECPublicKey.fromBytes(ByteVector(bytes))
  }

  def validatePublicKey(bytes: ByteVector): Boolean = {
    Try(decodePoint(bytes))
      .map(_.getCurve == curve)
      .getOrElse(false)
  }

  def pubKeyTweakMul(pubKey: ECPublicKey, tweak: FieldElement): ECPublicKey = {
    val tweakedPoint = decodePoint(pubKey).multiply(tweak.toBigInteger)
    decodePubKey(tweakedPoint, pubKey.isCompressed)
  }

  def decompressPublicKey(publicKey: ECPublicKey): ECPublicKey = {
    if (publicKey.isCompressed) {
      val point = decodePoint(publicKey.bytes)
      val decompressedBytes =
        ByteVector.fromHex("04").get ++
          ByteVector(point.getXCoord.getEncoded) ++
          ByteVector(point.getYCoord.getEncoded)
      ECPublicKey(decompressedBytes)
    } else publicKey
  }

  def computePublicKey(privateKey: ECPrivateKey): ECPublicKey = {
    val priv = getBigInteger(privateKey.bytes)
    val point = G.multiply(priv)
    val pubBytes = ByteVector(point.getEncoded(privateKey.isCompressed))
    require(
      ECPublicKey.isFullyValid(pubBytes),
      s"Bouncy Castle failed to generate a valid public key, got: ${CryptoBytesUtil
        .encodeHex(pubBytes)}")
    ECPublicKey(pubBytes)
  }

  def sign(
      dataToSign: ByteVector,
      privateKey: ECPrivateKey): ECDigitalSignature = {
    val signer: ECDSASigner = new ECDSASigner(
      new HMacDSAKCalculator(new SHA256Digest()))
    val privKey: ECPrivateKeyParameters =
      new ECPrivateKeyParameters(getBigInteger(privateKey.bytes),
                                 BouncyCastleCryptoParams.curve)
    signer.init(true, privKey)
    val components: Array[BigInteger] =
      signer.generateSignature(dataToSign.toArray)
    val (r, s) = (components(0), components(1))
    val signature = ECDigitalSignature(r, s)
    //make sure the signature follows BIP62's low-s value
    //https://github.com/bitcoin/bips/blob/master/bip-0062.mediawiki#Low_S_values_in_signatures
    //bitcoinj implementation
    //https://github.com/bitcoinj/bitcoinj/blob/1e66b9a8e38d9ad425507bf5f34d64c5d3d23bb8/core/src/main/java/org/bitcoinj/core/ECKey.java#L551
    val signatureLowS = DERSignatureUtil.lowS(signature)
    require(
      signatureLowS.isDEREncoded,
      "We must create DER encoded signatures when signing a piece of data, got: " + signatureLowS)
    signatureLowS
  }

  /** Create an ECDSA signature adding specified entropy.
    *
    * This can be used to include your own entropy to nonce generation
    * in addition to the message and private key, while still doing so deterministically.
    *
    * In particular, this is used when generating low R signatures.
    * @see [[https://github.com/bitcoin/bitcoin/pull/13666/]]
    */
  def signWithEntropy(
      dataToSign: ByteVector,
      privateKey: ECPrivateKey,
      entropy: ByteVector): ECDigitalSignature = {
    val signer: ECDSASigner = new ECDSASigner(
      new HMacDSAKCalculatorWithEntropy(new SHA256Digest(), entropy))
    val privKey: ECPrivateKeyParameters =
      new ECPrivateKeyParameters(getBigInteger(privateKey.bytes),
                                 BouncyCastleCryptoParams.curve)
    signer.init(true, privKey)
    val components: Array[BigInteger] =
      signer.generateSignature(dataToSign.toArray)
    val (r, s) = (components(0), components(1))
    val signature = ECDigitalSignature(r, s)
    //make sure the signature follows BIP62's low-s value
    //https://github.com/bitcoin/bips/blob/master/bip-0062.mediawiki#Low_S_values_in_signatures
    //bitcoinj implementation
    //https://github.com/bitcoinj/bitcoinj/blob/1e66b9a8e38d9ad425507bf5f34d64c5d3d23bb8/core/src/main/java/org/bitcoinj/core/ECKey.java#L551
    val signatureLowS = DERSignatureUtil.lowS(signature)
    require(
      signatureLowS.isDEREncoded,
      "We must create DER encoded signatures when signing a piece of data, got: " + signatureLowS)
    signatureLowS
  }

  def verifyDigitalSignature(
      data: ByteVector,
      publicKey: ECPublicKey,
      signature: ECDigitalSignature): Boolean = {
    val resultTry = Try {
      val publicKeyParams =
        new ECPublicKeyParameters(decodePoint(publicKey.bytes),
                                  BouncyCastleCryptoParams.curve)

      val signer = new ECDSASigner
      signer.init(false, publicKeyParams)
      signature match {
        case EmptyDigitalSignature =>
          signer.verifySignature(data.toArray,
                                 java.math.BigInteger.valueOf(0),
                                 java.math.BigInteger.valueOf(0))
        case _: ECDigitalSignature =>
          val (r, s) = signature.decodeSignature
          signer.verifySignature(data.toArray, r.bigInteger, s.bigInteger)
      }
    }
    resultTry.getOrElse(false)
  }
}

object AdaptorStuff {
  import ECAdaptorSignature.{deserializePoint, serializePoint}

  // Compute s' = k^-1 * (dataToSign + rx*privateKey)
  private def adaptorSignHelper(
      dataToSign: ByteVector,
      k: FieldElement,
      r: ECPublicKey,
      privateKey: ECPrivateKey): FieldElement = {
    val rx = FieldElement(
      BouncyCastleUtil.decodePoint(r).getXCoord.toBigInteger)
    val x = privateKey.fieldElement
    val m = FieldElement(dataToSign)
    val kInv = k.inverse

    rx.multiply(x).add(m).multiply(kInv)
  }

  def adaptorSign(
      privateKey: ECPrivateKey,
      adaptorPoint: ECPublicKey,
      dataToSign: ByteVector): ECAdaptorSignature = {
    // Include dataToSign and adaptor in nonce derivation
    val hash =
      BouncycastleCryptoRuntime.sha256(
        dataToSign ++ serializePoint(adaptorPoint))
    val k = DLEQStuff.dleqNonceFunc(hash.bytes,
                                    privateKey.fieldElement,
                                    "ECDSAAdaptorNon")

    if (k.isZero) {
      throw new RuntimeException("Nonce cannot be zero.")
    }

    val untweakedNonce = k.getPublicKey // k*G
    val tweakedNonce = adaptorPoint.tweakMultiply(k) // k*Y

    // DLEQ_prove((G,R'),(Y, R))
    val (proofS, proofE) =
      DLEQStuff.dleqProve(k, adaptorPoint, "ECDSAAdaptorSig")

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

    val validProof = DLEQStuff.dleqVerify(
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

object DLEQStuff {
  import ECAdaptorSignature.serializePoint

  def dleqPair(
      fe: FieldElement,
      adaptorPoint: ECPublicKey): (ECPublicKey, ECPublicKey) = {
    val point = fe.getPublicKey
    val tweakedPoint = adaptorPoint.tweakMultiply(fe)

    (point, tweakedPoint)
  }

  def dleqNonceFunc(
      hash: ByteVector,
      fe: FieldElement,
      algoName: String): FieldElement = {
    val kBytes =
      BouncycastleCryptoRuntime.taggedSha256(fe.bytes ++ hash, algoName).bytes
    FieldElement(kBytes)
  }

  def dleqChallengeHash(
      algoName: String,
      adaptorPoint: ECPublicKey,
      r1: ECPublicKey,
      r2: ECPublicKey,
      p1: ECPublicKey,
      p2: ECPublicKey): ByteVector = {
    BouncycastleCryptoRuntime
      .taggedSha256(
        serializePoint(adaptorPoint) ++ serializePoint(r1) ++ serializePoint(
          r2) ++ serializePoint(p1) ++ serializePoint(p2),
        algoName)
      .bytes
  }

  /** Proves that the DLOG_G(R') = DLOG_Y(R) (= fe)
    * For a full description, see https://cs.nyu.edu/courses/spring07/G22.3220-001/lec3.pdf
    */
  def dleqProve(
      fe: FieldElement,
      adaptorPoint: ECPublicKey,
      algoName: String): (FieldElement, FieldElement) = {
    // (fe*G, fe*Y)
    val (p1, p2) = dleqPair(fe, adaptorPoint)

    // hash(Y || fe*G || fe*Y)
    val hash =
      BouncycastleCryptoRuntime
        .sha256(
          serializePoint(adaptorPoint) ++ serializePoint(p1) ++ serializePoint(
            p2))
        .bytes
    val k = dleqNonceFunc(hash, fe, algoName)

    if (k.isZero) {
      throw new RuntimeException("Nonce cannot be zero.")
    }

    val r1 = k.getPublicKey
    val r2 = adaptorPoint.tweakMultiply(k)

    // Hash all components to get a challenge (this is the trick that turns
    // interactive ZKPs into non-interactive ZKPs, using hash assumptions)
    //
    // In short, rather than having the verifier present challenges, hash
    // all shared information (so that both parties can compute) and use
    // this hash as the challenge to the prover as loosely speaking this
    // should only be game-able if the prover can reverse hash functions.
    val challengeHash =
      dleqChallengeHash(algoName, adaptorPoint, r1, r2, p1, p2)
    val e = FieldElement(challengeHash)

    // s = k + fe*challenge. This proof works because then k = fe*challenge - s
    // so that R' = k*G =?= p1*challenge - s and R = k*Y =?= p2*challenge - s
    // can both be verified given s and challenge and will be true if and only
    // if R = y*R' which is what we are trying to prove.
    val s = fe.multiply(e).add(k)

    (s, e)
  }

  /** Verifies a proof that the DLOG_G of P1 equals the DLOG_adaptor of P2 */
  def dleqVerify(
      algoName: String,
      s: FieldElement,
      e: FieldElement,
      p1: ECPublicKey,
      adaptor: ECPublicKey,
      p2: ECPublicKey): Boolean = {
    val r1 = p1.tweakMultiply(e.negate).add(s.getPublicKey)
    val r2 = p2.tweakMultiply(e.negate).add(adaptor.tweakMultiply(s))
    val challengeHash = dleqChallengeHash(algoName, adaptor, r1, r2, p1, p2)

    challengeHash == e.bytes
  }
}
