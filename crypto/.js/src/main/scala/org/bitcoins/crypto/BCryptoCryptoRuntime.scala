package org.bitcoins.crypto

import org.bitcoins.crypto.facade.{
  Buffer,
  ECDSA,
  HMAC,
  Hash160,
  Random,
  RandomBrowser,
  RipeMd160,
  SHA1,
  SHA256,
  SHA256Factory,
  SHA512,
  SipHash
}
import scodec.bits.ByteVector

import java.math.BigInteger
import scala.scalajs.js
import scala.scalajs.js.JSStringOps._
import scala.scalajs.js.{JavaScriptException, UnicodeNormalizationForm}

/** This is an implementation of [[CryptoRuntime]] that defaults to
  * Bcrypto (https://github.com/bcoin-org/bcrypto) when possible.
  */
trait BCryptoCryptoRuntime extends CryptoRuntime {
  override val cryptoContext: CryptoContext = CryptoContext.BCrypto

  private lazy val hash160 = new Hash160
  private lazy val ripeMd160 = new RipeMd160
  private lazy val sha1 = new SHA1
  private lazy val sha256 = SHA256Factory.create()
  private lazy val hmac = SHA512.hmac.apply().asInstanceOf[HMAC]

  private lazy val ecdsa =
    new ECDSA("SECP256K1", sha256, js.constructorOf[SHA256], null)

  private lazy val randomBytesFunc: Int => ByteVector = { int =>
    try {
      // try to call the native implementation
      Random.randomBytes(1)
      CryptoJsUtil.toByteVector(Random.randomBytes(int))
    } catch {
      case _: Throwable =>
        // the native implementation is not available,
        // fall back to the JS implementation
        val bytesBuffer = RandomBrowser.randomBytes(int)
        CryptoJsUtil.toByteVector(bytesBuffer)
    }
  }

  def randomBytes(n: Int): ByteVector = randomBytesFunc(n)

  override def ripeMd160(bytes: ByteVector): RipeMd160Digest = {
    val buffer = CryptoJsUtil.toNodeBuffer(bytes)
    ripeMd160.init()
    ripeMd160.update(buffer)
    val hashBytes = ripeMd160.`final`()
    val hashByteVec = CryptoJsUtil.toByteVector(hashBytes)
    RipeMd160Digest.fromBytes(hashByteVec)
  }

  override def sha256Hash160(bytes: ByteVector): Sha256Hash160Digest = {
    val buffer = CryptoJsUtil.toNodeBuffer(bytes)
    hash160.init()
    hash160.update(buffer)
    val hashBytes = hash160.`final`()
    val hashBytesVec = CryptoJsUtil.toByteVector(hashBytes)
    Sha256Hash160Digest.fromBytes(hashBytesVec)
  }

  /** Converts a private key -> public key
    *
    * @param privateKey   the private key we want the corresponding public key for
    * @param isCompressed whether the returned public key should be compressed or not
    */
  override def toPublicKey(privateKey: ECPrivateKey): ECPublicKey = {
    val buffer = CryptoJsUtil.toNodeBuffer(privateKey.bytes)
    val pubKeyBuffer =
      ecdsa.publicKeyCreate(key = buffer, compressed = privateKey.isCompressed)
    val privKeyByteVec = CryptoJsUtil.toByteVector(pubKeyBuffer)
    ECPublicKey.fromBytes(privKeyByteVec)
  }

  override def sha256(bytes: ByteVector): Sha256Digest = {
    val buffer = CryptoJsUtil.toNodeBuffer(bytes)
    sha256.init()
    sha256.update(buffer)
    val hashBuffer = sha256.`final`()
    val hashByteVec = CryptoJsUtil.toByteVector(hashBuffer)
    Sha256Digest.fromBytes(hashByteVec)
  }

  /** Generates a 32 byte private key */
  override def freshPrivateKey: ECPrivateKey = {
    val keyBytes = ecdsa.privateKeyGenerate()
    val byteVec = CryptoJsUtil.toByteVector(keyBytes)
    ECPrivateKey.fromBytes(byteVec)
  }

  override def sha1(bytes: ByteVector): Sha1Digest = {
    val buffer = CryptoJsUtil.toNodeBuffer(bytes)
    sha1.init()
    sha1.update(buffer)
    val hashBuffer = sha1.`final`()
    val hashByteVec = CryptoJsUtil.toByteVector(hashBuffer)
    val result = Sha1Digest.fromBytes(hashByteVec)
    result
  }

  override def hmac512(key: ByteVector, data: ByteVector): ByteVector = {
    val keyBuffer = CryptoJsUtil.toNodeBuffer(key)
    val dataBuffer = CryptoJsUtil.toNodeBuffer(data)
    hmac.init(keyBuffer)
    hmac.update(dataBuffer)
    val result = hmac.`final`()
    CryptoJsUtil.toByteVector(result)
  }

  override def normalize(str: String): String =
    str.normalize(UnicodeNormalizationForm.NFC)

  /** Recover public keys from a signature and the message that was signed. This method will return 2 public keys, and the signature
    * can be verified with both, but only one of them matches that private key that was used to generate the signature.
    *
    * @param signature signature
    * @param message   message that was signed
    * @return a (pub1, pub2) tuple where pub1 and pub2 are candidates public keys. If you have the recovery id  then use
    *         pub1 if the recovery id is even and pub2 if it is odd
    */
  override def recoverPublicKey(
      signature: ECDigitalSignature,
      message: ByteVector): (ECPublicKey, ECPublicKey) = {
    val msgBuffer = CryptoJsUtil.toNodeBuffer(message)
    val sigBuffer = CryptoJsUtil.toNodeBuffer(signature.bytes)
    val keyBytes =
      if (signature.isDEREncoded)
        ecdsa.recoverDER(msgBuffer, sigBuffer, param = 0, compress = true)
      else
        ecdsa.recover(msgBuffer, sigBuffer, param = 0, compress = true)

    val keyByteVec = CryptoJsUtil.toByteVector(keyBytes)
    val key = ECPublicKey.fromBytes(keyByteVec)

    val keyBytesWithSign =
      if (signature.isDEREncoded)
        ecdsa.recoverDER(msgBuffer, sigBuffer, param = 1, compress = true)
      else
        ecdsa.recover(msgBuffer, sigBuffer, param = 1, compress = true)

    val keyWithSignByteVec = CryptoJsUtil.toByteVector(keyBytesWithSign)
    val keyWithSign = ECPublicKey.fromBytes(keyWithSignByteVec)

    (key, keyWithSign)
  }

  override def publicKey(privateKey: ECPrivateKey): ECPublicKey = {
    val buffer = CryptoJsUtil.toNodeBuffer(privateKey.bytes)
    val bufferPubKey =
      ecdsa.publicKeyCreate(buffer, privateKey.isCompressed)
    val byteVec = CryptoJsUtil.toByteVector(bufferPubKey)
    ECPublicKey.fromBytes(byteVec)
  }

  override def sign(
      privateKey: ECPrivateKey,
      dataToSign: ByteVector): ECDigitalSignature = {
    val privBuffer = CryptoJsUtil.toNodeBuffer(privateKey.bytes)
    val dataBuffer = CryptoJsUtil.toNodeBuffer(dataToSign)
    val buffer = ecdsa.signDER(dataBuffer, privBuffer)
    val byteVec = CryptoJsUtil.toByteVector(buffer)
    ECDigitalSignature.fromFrontOfBytes(byteVec)
  }

  override def signWithEntropy(
      privateKey: ECPrivateKey,
      bytes: ByteVector,
      entropy: ByteVector): ECDigitalSignature = ???

  override def secKeyVerify(privateKeybytes: ByteVector): Boolean = {
    val buffer = CryptoJsUtil.toNodeBuffer(privateKeybytes)
    ecdsa.privateKeyVerify(buffer)
  }

  override def verify(
      publicKey: ECPublicKey,
      data: ByteVector,
      signature: ECDigitalSignature): Boolean = {
    val dataBuffer = CryptoJsUtil.toNodeBuffer(data)
    val sigBuffer = CryptoJsUtil.toNodeBuffer(signature.bytes)
    val pubKeyBuffer = CryptoJsUtil.toNodeBuffer(publicKey.bytes)
    if (signature.isDEREncoded) {
      ecdsa.verifyDER(dataBuffer, sigBuffer, pubKeyBuffer)
    } else {
      ecdsa.verify(dataBuffer, sigBuffer, pubKeyBuffer)
    }
  }

  override def tweakMultiply(
      publicKey: ECPublicKey,
      tweak: FieldElement): ECPublicKey = {
    val pubKeyBuffer = CryptoJsUtil.toNodeBuffer(publicKey.bytes)
    val tweakBuffer = CryptoJsUtil.toNodeBuffer(tweak.bytes)
    val keyBuffer = ecdsa.publicKeyTweakMul(pubKeyBuffer, tweakBuffer, true)
    val keyByteVec = CryptoJsUtil.toByteVector(keyBuffer)
    ECPublicKey.fromBytes(keyByteVec)
  }

  def publicKeyConvert(buffer: ByteVector, compressed: Boolean): ByteVector = {
    val pubKeyBuffer =
      publicKeyConvert(CryptoJsUtil.toNodeBuffer(buffer), compressed)
    CryptoJsUtil.toByteVector(pubKeyBuffer)
  }

  def publicKeyConvert(buffer: Buffer, compressed: Boolean): Buffer =
    ecdsa.publicKeyConvert(buffer, compressed)

  override def publicKeyConvert(
      key: ECPublicKey,
      compressed: Boolean): ECPublicKey =
    ECPublicKey(publicKeyConvert(key.bytes, compressed))

  override def add(pk1: ECPublicKey, pk2: ECPublicKey): ECPublicKey = {
    val pk1Buffer = CryptoJsUtil.toNodeBuffer(pk1.bytes)
    val pk2Buffer = CryptoJsUtil.toNodeBuffer(pk2.bytes)
    try {
      val keyBuffer =
        ecdsa.publicKeyCombine(js.Array(pk1Buffer, pk2Buffer), true)
      val keyBytes = CryptoJsUtil.toByteVector(keyBuffer)
      ECPublicKey.fromBytes(keyBytes)
    } catch {
      case ex: JavaScriptException =>
        // check for infinity
        val k1: ByteVector =
          if (pk1.isCompressed) pk1.bytes
          else publicKeyConvert(pk1.bytes, compressed = true)

        val k2: ByteVector =
          if (pk2.isCompressed) pk2.bytes
          else publicKeyConvert(pk2.bytes, compressed = true)

        if (
          ((k1.head == 0x02 && k2.head == 0x03) ||
            (k1.head == 0x03 && k2.head == 0x02)) &&
          k1.tail == k2.tail
        ) {
          ECPublicKey.infinity
        } else {
          throw ex
        }
    }
  }

  override def pubKeyTweakAdd(
      pubkey: ECPublicKey,
      privkey: ECPrivateKey): ECPublicKey = {
    val pubKeyBuffer = CryptoJsUtil.toNodeBuffer(pubkey.bytes)
    val privKeyBuffer = CryptoJsUtil.toNodeBuffer(privkey.bytes)
    val keyBuffer = ecdsa.publicKeyTweakAdd(pubKeyBuffer, privKeyBuffer, true)
    val keyByteVec = CryptoJsUtil.toByteVector(keyBuffer)
    ECPublicKey.fromBytes(keyByteVec)
  }

  override def isValidPubKey(bytes: ByteVector): Boolean = {
    val buffer = CryptoJsUtil.toNodeBuffer(bytes)
    ecdsa.publicKeyVerify(buffer)
  }

  override def sipHash(item: ByteVector, key: SipHashKey): Long = {
    val itemBuffer = CryptoJsUtil.toNodeBuffer(item)
    val keyBuffer = CryptoJsUtil.toNodeBuffer(key.bytes)
    val siphash = SipHash.siphash(itemBuffer, keyBuffer)
    val hi = (siphash(0).toLong & 0x00000000ffffffffL) << 32
    val lo = siphash(1).toLong & 0x00000000ffffffffL
    hi | lo
  }

  override def decodePoint(bytes: ByteVector): ECPoint = {
    if (bytes.size == 1 && bytes(0) == 0x00) {
      ECPointInfinity
    } else {
      val decoded = ecdsa.curve
        .applyDynamic("decodePoint")(CryptoJsUtil.toNodeBuffer(bytes))
        .asInstanceOf[Point]

      if (decoded.isInfinity())
        ECPointInfinity
      else
        ECPoint(new BigInteger(decoded.getX().toString()),
                new BigInteger(decoded.getY().toString()))
    }
  }
}

object BCryptoCryptoRuntime extends BCryptoCryptoRuntime
