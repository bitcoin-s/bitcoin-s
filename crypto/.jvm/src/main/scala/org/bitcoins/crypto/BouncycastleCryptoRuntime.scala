package org.bitcoins.crypto

import org.bouncycastle.crypto.AsymmetricCipherKeyPair
import org.bouncycastle.crypto.digests.{RIPEMD160Digest, SHA512Digest}
import org.bouncycastle.crypto.generators.ECKeyPairGenerator
import org.bouncycastle.crypto.macs.{HMac, SipHash}
import org.bouncycastle.crypto.params.{
  ECKeyGenerationParameters,
  ECPrivateKeyParameters,
  KeyParameter
}
import org.bouncycastle.math.ec.ECPoint
import scodec.bits.ByteVector

import java.math.BigInteger
import java.security.{MessageDigest, SecureRandom}
import scala.util.{Failure, Success, Try}

/** This is an implementation of [[CryptoRuntime]] that defaults to Bouncy Castle (https://bouncycastle.org/)
  * and [[java.security]].
  */
trait BouncycastleCryptoRuntime extends CryptoRuntime {
  private[this] lazy val secureRandom = new SecureRandom()

  override val cryptoContext: CryptoContext = CryptoContext.BouncyCastle

  override def freshPrivateKey: ECPrivateKey = {
    val generator: ECKeyPairGenerator = new ECKeyPairGenerator
    val keyGenParams: ECKeyGenerationParameters =
      new ECKeyGenerationParameters(CryptoParams.curve, secureRandom)
    generator.init(keyGenParams)
    val keypair: AsymmetricCipherKeyPair = generator.generateKeyPair
    val privParams: ECPrivateKeyParameters =
      keypair.getPrivate.asInstanceOf[ECPrivateKeyParameters]
    val priv: BigInteger = privParams.getD
    val bytes = ByteVector(priv.toByteArray)
    ECPrivateKey.fromBytes(bytes)
  }

  /** @param x x coordinate
    * @return a tuple (p1, p2) where p1 and p2 are points on the curve and p1.x = p2.x = x
    *         p1.y is even, p2.y is odd
    */
  def recoverPoint(x: BigInteger): (ECPoint, ECPoint) = {
    val bytes = ByteVector(x.toByteArray)

    val bytes32 = if (bytes.length < 32) {
      bytes.padLeft(32)
    } else if (bytes.length == 32) {
      bytes
    } else if (bytes.length == 33 && bytes.head == 0.toByte) {
      bytes.tail
    } else {
      throw new IllegalArgumentException(
        s"Field element cannot have more than 32 bytes, got $bytes from $x")
    }

    (BouncyCastleUtil.decodePoint(ECPublicKey(0x02.toByte +: bytes32)),
     BouncyCastleUtil.decodePoint(ECPublicKey(0x03.toByte +: bytes32)))
  }

  override def recoverPublicKey(
      signature: ECDigitalSignature,
      message: ByteVector): (ECPublicKey, ECPublicKey) = {

    val curve = CryptoParams.curve
    val (r, s) = (signature.r.bigInteger, signature.s.bigInteger)

    val m = new BigInteger(1, message.toArray)

    val (p1, p2) = recoverPoint(r)

    val Q1 = p1
      .multiply(s)
      .subtract(curve.getG.multiply(m))
      .multiply(r.modInverse(curve.getN))
    val Q2 = p2
      .multiply(s)
      .subtract(curve.getG.multiply(m))
      .multiply(r.modInverse(curve.getN))

    val pub1 = BouncyCastleUtil.decodePubKey(Q1)
    val pub2 = BouncyCastleUtil.decodePubKey(Q2)
    (pub1, pub2)
  }

  override def hmac512(key: ByteVector, data: ByteVector): ByteVector = {
    val hmac512 = new HMac(new SHA512Digest())
    hmac512.init(new KeyParameter(key.toArray))
    hmac512.update(data.toArray, 0, data.intSize.get)
    val output = new Array[Byte](64)
    hmac512.doFinal(output, 0)
    ByteVector(output)
  }

  override def ripeMd160(bytes: ByteVector): RipeMd160Digest = {
    //from this tutorial http://rosettacode.org/wiki/RIPEMD-160#Scala
    val messageDigest = new RIPEMD160Digest
    val raw = bytes.toArray
    messageDigest.update(raw, 0, raw.length)
    val out = Array.fill[Byte](messageDigest.getDigestSize)(0)
    messageDigest.doFinal(out, 0)
    RipeMd160Digest(ByteVector(out))
  }

  override def sha256(bytes: ByteVector): Sha256Digest = {
    val hash = MessageDigest.getInstance("SHA-256").digest(bytes.toArray)
    Sha256Digest(ByteVector(hash))
  }

  override def sha1(bytes: ByteVector): Sha1Digest = {
    val hash = MessageDigest.getInstance("SHA-1").digest(bytes.toArray).toList
    Sha1Digest(ByteVector(hash))
  }

  override def normalize(str: String): String = {
    java.text.Normalizer.normalize(str, java.text.Normalizer.Form.NFC)
  }

  override def sha256Hash160(bytes: ByteVector): Sha256Hash160Digest = {
    val hash = ripeMd160(sha256(bytes).bytes).bytes
    Sha256Hash160Digest(hash)
  }

  override def toPublicKey(
      privateKey: ECPrivateKey,
      isCompressed: Boolean): ECPublicKey =
    BouncyCastleUtil.computePublicKey(privateKey)

  override def sign(
      privateKey: ECPrivateKey,
      dataToSign: ByteVector): ECDigitalSignature = {
    BouncyCastleUtil.sign(dataToSign, privateKey)
  }

  override def signWithEntropy(
      privateKey: ECPrivateKey,
      bytes: ByteVector,
      entropy: ByteVector): ECDigitalSignature =
    BouncyCastleUtil.signWithEntropy(bytes, privateKey, entropy)

  override def secKeyVerify(privateKeyBytes: ByteVector): Boolean =
    CryptoParams.curve.getCurve
      .isValidFieldElement(new BigInteger(1, privateKeyBytes.toArray))

  override def verify(
      publicKey: ECPublicKey,
      data: ByteVector,
      signature: ECDigitalSignature): Boolean =
    BouncyCastleUtil.verifyDigitalSignature(data, publicKey, signature)

  override def decompressed(publicKey: ECPublicKey): ECPublicKey =
    BouncyCastleUtil.decompressPublicKey(publicKey)

  override def publicKey(privateKey: ECPrivateKey): ECPublicKey =
    BouncyCastleUtil.computePublicKey(privateKey)

  override def tweakMultiply(
      publicKey: ECPublicKey,
      tweak: FieldElement): ECPublicKey =
    BouncyCastleUtil.pubKeyTweakMul(publicKey, tweak.bytes)

  override def add(pk1: ECPrivateKey, pk2: ECPrivateKey): ECPrivateKey =
    pk1.fieldElement.add(pk2.fieldElement).toPrivateKey

  override def add(pk1: ByteVector, pk2: ECPrivateKey): ByteVector = {
    val sum = pk2.fieldElement.add(FieldElement(pk1))
    sum.bytes
  }

  override def add(pk1: ECPublicKey, pk2: ECPublicKey): ECPublicKey = {
    val sumPoint =
      BouncyCastleUtil.decodePoint(pk1).add(BouncyCastleUtil.decodePoint(pk2))

    BouncyCastleUtil.decodePubKey(sumPoint)
  }

  def pubKeyTweakAdd(
      pubkey: ECPublicKey,
      privkey: ECPrivateKey): ECPublicKey = {
    val tweak = privkey.publicKey
    pubkey.add(tweak)
  }

  override def isValidPubKey(bytes: ByteVector): Boolean =
    BouncyCastleUtil.validatePublicKey(bytes)

  override def isFullyValidWithBouncyCastle(bytes: ByteVector): Boolean =
    bytes.nonEmpty && isValidPubKey(bytes)

  override def schnorrSign(
      dataToSign: ByteVector,
      privateKey: ECPrivateKey,
      auxRand: ByteVector): SchnorrDigitalSignature = {
    val nonceKey =
      SchnorrNonce.kFromBipSchnorr(privateKey, dataToSign, auxRand)

    schnorrSignWithNonce(dataToSign, privateKey, nonceKey)
  }

  override def schnorrSignWithNonce(
      dataToSign: ByteVector,
      privateKey: ECPrivateKey,
      nonceKey: ECPrivateKey): SchnorrDigitalSignature = {
    val rx = nonceKey.schnorrNonce
    val k = nonceKey.nonceKey.fieldElement
    val x = privateKey.schnorrKey.fieldElement
    val e = sha256SchnorrChallenge(
      rx.bytes ++ privateKey.schnorrPublicKey.bytes ++ dataToSign).bytes

    val challenge = x.multiply(FieldElement(e))
    val sig = k.add(challenge)

    SchnorrDigitalSignature(rx, sig)
  }

  override def schnorrVerify(
      data: ByteVector,
      schnorrPubKey: SchnorrPublicKey,
      signature: SchnorrDigitalSignature): Boolean = {
    val rx = signature.rx
    val sT = Try(signature.sig.toPrivateKey)

    sT match {
      case Success(s) =>
        val eBytes = sha256SchnorrChallenge(
          rx.bytes ++ schnorrPubKey.bytes ++ data).bytes

        val e = FieldElement(eBytes)
        val negE = e.negate

        val sigPoint = s.publicKey
        val challengePoint = schnorrPubKey.publicKey.tweakMultiply(negE)
        val computedR = challengePoint.add(sigPoint)
        val yCoord = BouncyCastleUtil.decodePoint(computedR).getRawYCoord

        yCoord != null && !yCoord.testBitZero() && computedR.schnorrNonce == rx
      case Failure(_) => false
    }
  }

  override def schnorrComputeSigPoint(
      data: ByteVector,
      nonce: SchnorrNonce,
      pubKey: SchnorrPublicKey,
      compressed: Boolean): ECPublicKey = {
    val eBytes = sha256SchnorrChallenge(
      nonce.bytes ++ pubKey.bytes ++ data).bytes

    val e = FieldElement(eBytes)

    val compressedSigPoint =
      nonce.publicKey.add(pubKey.publicKey.tweakMultiply(e))

    if (compressed) {
      compressedSigPoint
    } else {
      compressedSigPoint.decompressed
    }
  }

  override def adaptorSign(
      key: ECPrivateKey,
      adaptorPoint: ECPublicKey,
      msg: ByteVector): ECAdaptorSignature = {
    AdaptorStuff.adaptorSign(key, adaptorPoint, msg)
  }

  override def adaptorComplete(
      key: ECPrivateKey,
      adaptorSignature: ECAdaptorSignature): ECDigitalSignature = {
    AdaptorStuff.adaptorComplete(key, adaptorSignature.adaptedSig)
  }

  override def extractAdaptorSecret(
      signature: ECDigitalSignature,
      adaptorSignature: ECAdaptorSignature,
      key: ECPublicKey): ECPrivateKey = {
    AdaptorStuff.extractAdaptorSecret(signature, adaptorSignature, key)
  }

  override def adaptorVerify(
      adaptorSignature: ECAdaptorSignature,
      key: ECPublicKey,
      msg: ByteVector,
      adaptorPoint: ECPublicKey): Boolean =
    AdaptorStuff.adaptorVerify(adaptorSignature, key, msg, adaptorPoint)

  override def decodeSignature(
      signature: ECDigitalSignature): (BigInt, BigInt) =
    DERSignatureUtil.decodeSignature(signature)

  override def isValidSignatureEncoding(
      signature: ECDigitalSignature): Boolean =
    DERSignatureUtil.isValidSignatureEncoding(signature)

  override def isDEREncoded(signature: ECDigitalSignature): Boolean =
    DERSignatureUtil.isDEREncoded(signature)

  override def sipHash(item: ByteVector, key: SipHashKey): Long = {
    val sipHashCParam = 2
    val sipHashDParam = 4

    val sh = new SipHash(sipHashCParam, sipHashDParam)

    val keyParam = new KeyParameter(key.bytes.toArray)

    sh.init(keyParam)

    val offset = 0

    sh.update(item.toArray, offset, item.length.toInt)

    sh.doFinal()
  }

}

object BouncycastleCryptoRuntime extends BouncycastleCryptoRuntime
