package org.bitcoins.crypto

import org.bitcoin.NativeSecp256k1
import org.bouncycastle.crypto.AsymmetricCipherKeyPair
import org.bouncycastle.crypto.digests.{RIPEMD160Digest, SHA512Digest}
import org.bouncycastle.crypto.generators.ECKeyPairGenerator
import org.bouncycastle.crypto.macs.HMac
import org.bouncycastle.crypto.params.{
  ECKeyGenerationParameters,
  ECPrivateKeyParameters,
  KeyParameter
}
import org.bouncycastle.math.ec.ECPoint
import scodec.bits.ByteVector

import java.math.BigInteger
import java.security.{MessageDigest, SecureRandom}

trait JvmCryptoRuntime extends CryptoRuntime {
  private[this] lazy val secureRandom = new SecureRandom()

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
  override def recoverPoint(x: BigInteger): (ECPoint, ECPoint) = {
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

    (ECPublicKey(0x02.toByte +: bytes32).toPoint,
     ECPublicKey(0x03.toByte +: bytes32).toPoint)
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

    val pub1 = ECPublicKey.fromPoint(Q1)
    val pub2 = ECPublicKey.fromPoint(Q2)
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
      isCompressed: Boolean): ECPublicKey = {
    CryptoContext.default match {
      case CryptoContext.BouncyCastle =>
        BouncyCastleUtil.computePublicKey(privateKey)
      case CryptoContext.LibSecp256k1 =>
        val pubKeyBytes: Array[Byte] =
          NativeSecp256k1.computePubkey(privateKey.bytes.toArray, isCompressed)
        val pubBytes = ByteVector(pubKeyBytes)
        require(
          ECPublicKey.isFullyValid(pubBytes),
          s"secp256k1 failed to generate a valid public key, got: ${CryptoBytesUtil
            .encodeHex(pubBytes)}")
        ECPublicKey(pubBytes)
    }
  }
}

object JvmCryptoRuntime extends JvmCryptoRuntime
