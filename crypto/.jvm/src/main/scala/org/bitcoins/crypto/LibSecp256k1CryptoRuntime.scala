package org.bitcoins.crypto

import org.bitcoin.NativeSecp256k1
import org.bouncycastle.math.ec.ECPoint
import scodec.bits.ByteVector

import java.math.BigInteger
import java.security.{MessageDigest, SecureRandom}

trait LibSecp256k1CryptoRuntime extends CryptoRuntime {
  private[this] lazy val secureRandom = new SecureRandom()

  override val cryptoContext: CryptoContext = CryptoContext.LibSecp256k1

  override def freshPrivateKey: ECPrivateKey =
    BouncycastleCryptoRuntime.freshPrivateKey

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

  override def hmac512(key: ByteVector, data: ByteVector): ByteVector =
    BouncycastleCryptoRuntime.hmac512(key, data)

  override def ripeMd160(bytes: ByteVector): RipeMd160Digest = {
    BouncycastleCryptoRuntime.ripeMd160(bytes)
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
    val pubKeyBytes: Array[Byte] =
      NativeSecp256k1.computePubkey(privateKey.bytes.toArray, isCompressed)
    val pubBytes = ByteVector(pubKeyBytes)
    require(
      ECPublicKey.isFullyValid(pubBytes),
      s"secp256k1 failed to generate a valid public key, got: ${CryptoBytesUtil
        .encodeHex(pubBytes)}")
    ECPublicKey(pubBytes)
  }

  override def sign(
      privateKey: ECPrivateKey,
      dataToSign: ByteVector): ECDigitalSignature = {
    val signature =
      NativeSecp256k1.sign(dataToSign.toArray, privateKey.bytes.toArray)
    ECDigitalSignature(ByteVector(signature))
  }

  override def signWithEntropy(
      privateKey: ECPrivateKey,
      bytes: ByteVector,
      entropy: ByteVector): ECDigitalSignature = {
    val sigBytes = NativeSecp256k1.signWithEntropy(bytes.toArray,
                                                   privateKey.bytes.toArray,
                                                   entropy.toArray)

    ECDigitalSignature(ByteVector(sigBytes))
  }

  override def secKeyVerify(privateKeyBytes: ByteVector): Boolean =
    NativeSecp256k1.secKeyVerify(privateKeyBytes.toArray)

  override def verify(
      publicKey: ECPublicKey,
      data: ByteVector,
      signature: ECDigitalSignature): Boolean = {
    val result =
      NativeSecp256k1.verify(data.toArray,
                             signature.bytes.toArray,
                             publicKey.bytes.toArray)

    if (!result) {
      //if signature verification fails with libsecp256k1 we need to use our old
      //verification function from spongy castle, this is needed because early blockchain
      //transactions can have weird non strict der encoded digital signatures
      //bitcoin core implements this functionality here:
      //https://github.com/bitcoin/bitcoin/blob/master/src/pubkey.cpp#L16-L165

      BouncycastleCryptoRuntime.verify(publicKey, data, signature)
    } else result
  }

  override def decompressed(publicKey: ECPublicKey): ECPublicKey = {
    if (publicKey.isCompressed) {
      val decompressed = NativeSecp256k1.decompress(publicKey.bytes.toArray)
      ECPublicKey.fromBytes(ByteVector(decompressed))
    } else publicKey
  }

  override def publicKey(privateKey: ECPrivateKey): ECPublicKey = {
    val pubKeyBytes: Array[Byte] =
      NativeSecp256k1.computePubkey(privateKey.bytes.toArray,
                                    privateKey.isCompressed)
    val pubBytes = ByteVector(pubKeyBytes)
    require(
      ECPublicKey.isFullyValid(pubBytes),
      s"secp256k1 failed to generate a valid public key, got: ${CryptoBytesUtil
        .encodeHex(pubBytes)}")
    ECPublicKey(pubBytes)
  }

  override def tweakMultiply(
      publicKey: ECPublicKey,
      tweak: FieldElement): ECPublicKey = {
    val mulBytes = NativeSecp256k1.pubKeyTweakMul(publicKey.bytes.toArray,
                                                  tweak.bytes.toArray,
                                                  publicKey.isCompressed)
    ECPublicKey(ByteVector(mulBytes))
  }

  override def add(pk1: ECPrivateKey, pk2: ECPrivateKey): ECPrivateKey = {
    val sumBytes =
      NativeSecp256k1.privKeyTweakAdd(pk1.bytes.toArray, pk2.bytes.toArray)
    ECPrivateKey(ByteVector(sumBytes))
  }

  override def add(pk1: ByteVector, pk2: ECPrivateKey): ByteVector = {
    val sum =
      NativeSecp256k1.privKeyTweakAdd(pk1.toArray, pk2.bytes.toArray)
    ByteVector(sum)
  }

  override def add(pk1: ECPublicKey, pk2: ECPublicKey): ECPublicKey =
    BouncycastleCryptoRuntime.add(pk1, pk2)

  override def pubKeyTweakAdd(
      pubkey: ECPublicKey,
      privkey: ECPrivateKey): ECPublicKey = {
    val tweaked = NativeSecp256k1.pubKeyTweakAdd(pubkey.bytes.toArray,
                                                 privkey.bytes.toArray,
                                                 privkey.isCompressed)
    ECPublicKey(ByteVector(tweaked))
  }

  override def isValidPubKey(bytes: ByteVector): Boolean = {
    try {
      NativeSecp256k1.isValidPubKey(bytes.toArray)
    } catch {
      case scala.util.control.NonFatal(_) =>
        false
    }
  }

  override def isFullyValidWithBouncyCastle(bytes: ByteVector): Boolean =
    bytes.nonEmpty && BouncycastleCryptoRuntime.isValidPubKey(bytes)

  // TODO: add native implementation
  override def schnorrSign(
      dataToSign: ByteVector,
      privateKey: ECPrivateKey,
      auxRand: ByteVector): SchnorrDigitalSignature = {
//    val sigBytes =
//      NativeSecp256k1.schnorrSign(dataToSign.toArray,
//                                  privateKey.bytes.toArray,
//                                  auxRand.toArray)
//    SchnorrDigitalSignature(ByteVector(sigBytes))
    BouncycastleCryptoRuntime.schnorrSign(dataToSign, privateKey, auxRand)
  }

  // TODO: add native implementation
  override def schnorrSignWithNonce(
      dataToSign: ByteVector,
      privateKey: ECPrivateKey,
      nonceKey: ECPrivateKey): SchnorrDigitalSignature = {
//    val sigBytes =
//      NativeSecp256k1.schnorrSignWithNonce(dataToSign.toArray,
//                                           privateKey.bytes.toArray,
//                                           nonceKey.bytes.toArray)
//    SchnorrDigitalSignature(ByteVector(sigBytes))
    BouncycastleCryptoRuntime.schnorrSignWithNonce(dataToSign,
                                                   privateKey,
                                                   nonceKey)
  }

  // TODO: add native implementation
  override def schnorrVerify(
      data: ByteVector,
      schnorrPubKey: SchnorrPublicKey,
      signature: SchnorrDigitalSignature): Boolean = {
    BouncycastleCryptoRuntime.schnorrVerify(data, schnorrPubKey, signature)
  }

  // TODO: add native implementation
  override def schnorrComputeSigPoint(
      data: ByteVector,
      nonce: SchnorrNonce,
      pubKey: SchnorrPublicKey,
      compressed: Boolean): ECPublicKey = {
    BouncycastleCryptoRuntime.schnorrComputeSigPoint(data,
                                                     nonce,
                                                     pubKey,
                                                     compressed)
  }

}

object LibSecp256k1CryptoRuntime extends LibSecp256k1CryptoRuntime
