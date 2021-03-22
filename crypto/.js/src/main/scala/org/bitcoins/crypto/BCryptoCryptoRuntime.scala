package org.bitcoins.crypto

import scodec.bits.ByteVector

import java.math.BigInteger
import scala.scalajs.js
import scala.scalajs.js.JSStringOps._
import scala.scalajs.js.typedarray._
import scala.scalajs.js.{JavaScriptException, UnicodeNormalizationForm}

/** This is an implementation of [[CryptoRuntime]] that defaults to
  * Bcrypto (https://github.com/bcoin-org/bcrypto) when possible.
  */
trait BCryptoCryptoRuntime extends CryptoRuntime {
  override val cryptoContext: CryptoContext = CryptoContext.BCrypto

  implicit def bufferToByteVector(b: Buffer): ByteVector = toByteVector(b)

  implicit def byteVectorToBuffer(b: ByteVector): Buffer = toNodeBuffer(b)

  private lazy val hash160 = new Hash160
  private lazy val ripeMd160 = new RipeMd160
  private lazy val sha1 = new SHA1
  private lazy val sha256 = SHA256Factory.create()
  private lazy val hmac = SHA512.hmac.apply().asInstanceOf[HMAC]

  private lazy val ecdsa =
    new ECDSA("SECP256K1", sha256, js.constructorOf[SHA256], null)

  private lazy val randomBytesFunc: Int => ByteVector =
    try {
      // try to call the native implementation
      Random.randomBytes(1)
      Random.randomBytes
    } catch {
      case _: Throwable =>
        // the native implementation is not available,
        // fall back to the JS implementation
        RandomBrowser.randomBytes
    }

  def randomBytes(n: Int): ByteVector = randomBytesFunc(n)

  override def ripeMd160(bytes: ByteVector): RipeMd160Digest = {
    ripeMd160.init()
    ripeMd160.update(bytes)
    val hashBytes = ripeMd160.`final`()
    RipeMd160Digest.fromBytes(hashBytes)
  }

  override def sha256Hash160(bytes: ByteVector): Sha256Hash160Digest = {
    hash160.init()
    hash160.update(bytes)
    val hashBytes = hash160.`final`()
    Sha256Hash160Digest.fromBytes(hashBytes)
  }

  /** Converts a private key -> public key
    *
    * @param privateKey   the private key we want the corresponding public key for
    * @param isCompressed whether the returned public key should be compressed or not
    */
  override def toPublicKey(privateKey: ECPrivateKey): ECPublicKey = {
    val pubKeyBuffer =
      ecdsa.publicKeyCreate(key = privateKey.bytes,
                            compressed = privateKey.isCompressed)
    ECPublicKey.fromBytes(pubKeyBuffer)
  }

  override def sha256(bytes: ByteVector): Sha256Digest = {
    sha256.init()
    sha256.update(bytes)
    val hashBytes = sha256.`final`()
    Sha256Digest.fromBytes(hashBytes)
  }

  /** Generates a 32 byte private key */
  override def freshPrivateKey: ECPrivateKey = {
    val keyBytes = ecdsa.privateKeyGenerate()
    ECPrivateKey.fromBytes(keyBytes)
  }

  override def sha1(bytes: ByteVector): Sha1Digest = {
    sha1.init()
    sha1.update(bytes)
    val hashBytes = sha1.`final`()
    Sha1Digest.fromBytes(hashBytes)
  }

  override def hmac512(key: ByteVector, data: ByteVector): ByteVector = {
    hmac.init(key)
    hmac.update(data)
    hmac.`final`()
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
    val keyBytes =
      if (signature.isDEREncoded)
        ecdsa.recoverDER(message, signature.bytes, param = 0, compress = true)
      else
        ecdsa.recover(message, signature.bytes, param = 0, compress = true)
    val key = ECPublicKey.fromBytes(keyBytes)

    val keyBytesWithSign =
      if (signature.isDEREncoded)
        ecdsa.recoverDER(message, signature.bytes, param = 1, compress = true)
      else
        ecdsa.recover(message, signature.bytes, param = 1, compress = true)
    val keyWithSign = ECPublicKey.fromBytes(keyBytesWithSign)

    (key, keyWithSign)
  }

  override def publicKey(privateKey: ECPrivateKey): ECPublicKey = {
    val buffer =
      ecdsa.publicKeyCreate(privateKey.bytes, privateKey.isCompressed)
    ECPublicKey.fromBytes(buffer)
  }

  override def sign(
      privateKey: ECPrivateKey,
      dataToSign: ByteVector): ECDigitalSignature = {
    val buffer = ecdsa.signDER(dataToSign, privateKey.bytes)
    ECDigitalSignature.fromFrontOfBytes(buffer)
  }

  override def signWithEntropy(
      privateKey: ECPrivateKey,
      bytes: ByteVector,
      entropy: ByteVector): ECDigitalSignature = ???

  override def secKeyVerify(privateKeybytes: ByteVector): Boolean =
    ecdsa.privateKeyVerify(privateKeybytes)

  override def verify(
      publicKey: ECPublicKey,
      data: ByteVector,
      signature: ECDigitalSignature): Boolean = {
    if (signature.isDEREncoded) {
      ecdsa.verifyDER(data, signature.bytes, publicKey.bytes)
    } else {
      ecdsa.verify(data, signature.bytes, publicKey.bytes)
    }
  }

  override def tweakMultiply(
      publicKey: ECPublicKey,
      tweak: FieldElement): ECPublicKey = {
    val keyBuffer = ecdsa.publicKeyTweakMul(publicKey.bytes, tweak.bytes, true)
    ECPublicKey.fromBytes(keyBuffer)
  }

  def publicKeyConvert(buffer: ByteVector, compressed: Boolean): ByteVector =
    publicKeyConvert(toNodeBuffer(buffer), compressed)

  def publicKeyConvert(buffer: Buffer, compressed: Boolean): Buffer =
    ecdsa.publicKeyConvert(buffer, compressed)

  override def publicKeyConvert(
      key: ECPublicKey,
      compressed: Boolean): ECPublicKey =
    ECPublicKey(publicKeyConvert(key.bytes, compressed))

  override def add(pk1: ECPublicKey, pk2: ECPublicKey): ECPublicKey = {
    try {
      val keyBuffer =
        ecdsa.publicKeyCombine(js.Array(pk1.bytes, pk2.bytes), true)
      ECPublicKey.fromBytes(keyBuffer)
    } catch {
      case ex: JavaScriptException =>
        // check for infinity
        val k1: Buffer =
          if (pk1.isCompressed) pk1.bytes
          else publicKeyConvert(pk1.bytes, compressed = true)

        val k2: Buffer =
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
    val keyBuffer = ecdsa.publicKeyTweakAdd(pubkey.bytes, privkey.bytes, true)
    ECPublicKey.fromBytes(keyBuffer)
  }

  override def isValidPubKey(bytes: ByteVector): Boolean =
    ecdsa.publicKeyVerify(bytes)

  override def sipHash(item: ByteVector, key: SipHashKey): Long = {
    val siphash = SipHash.siphash(item, key.bytes)
    val hi = (siphash(0).toLong & 0x00000000ffffffffL) << 32
    val lo = siphash(1).toLong & 0x00000000ffffffffL
    hi | lo
  }

  private def toNodeBuffer(byteVector: ByteVector): Buffer = {
    //the implicit used here is this
    //https://github.com/scala-js/scala-js/blob/b5a93bb99a0b0b5044141d4b2871ea260ef17798/library/src/main/scala/scala/scalajs/js/typedarray/package.scala#L33
    Buffer.from(byteVector.toArray.toTypedArray.buffer)
  }

  private def toByteVector(buffer: Buffer): ByteVector =
    toByteVector(buffer, buffer.length)

  private def toByteVector(buffer: Buffer, len: Int): ByteVector = {
    //is this right?
    val iter: js.Iterator[Int] = buffer.values()

    val accum = new scala.collection.mutable.ArrayBuffer[Int](len)

    var done = false
    while (!done) {
      val entry = iter.next()
      if (entry.done) {
        done = true
      } else {
        accum += entry.value
      }
    }
    require(accum.length == len,
            s"Need $len bytes for buffer -> bytevector conversion")
    ByteVector(accum.map(_.toByte))
  }

  override def decodePoint(bytes: ByteVector): ECPoint = {
    if (bytes.size == 1 && bytes(0) == 0x00) {
      ECPointInfinity
    } else {
      val decoded = ecdsa.curve
        .applyDynamic("decodePoint")(toNodeBuffer(bytes))
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
