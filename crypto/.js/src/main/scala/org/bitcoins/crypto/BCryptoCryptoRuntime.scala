package org.bitcoins.crypto

import org.bouncycastle.math.ec.ECPoint
import scodec.bits.ByteVector

import scala.scalajs.js
import scala.scalajs.js.annotation._
import scala.scalajs.js._
import scala.scalajs.js.typedarray._
import java.math.BigInteger
import java.nio.ByteBuffer
import scala.collection.mutable
import scodec.bits.ByteVector

trait BCryptoCryptoRuntime extends CryptoRuntime {
  override val cryptoContext: CryptoContext = CryptoContext.BCrypto

  /** Generates a 32 byte private key */
  override def freshPrivateKey: ECPrivateKey = ???

  /** Converts a private key -> public key
    *
    * @param privateKey   the private key we want the corresponding public key for
    * @param isCompressed whether the returned public key should be compressed or not
    */
  override def toPublicKey(
      privateKey: ECPrivateKey,
      isCompressed: Boolean): ECPublicKey = ???

  override def ripeMd160(bytes: ByteVector): RipeMd160Digest = ???

  override def sha256Hash160(bytes: ByteVector): Sha256Hash160Digest = ???

  override def sha256(bytes: ByteVector): Sha256Digest = ???

  override def sha1(bytes: ByteVector): Sha1Digest = ???

  override def hmac512(key: ByteVector, data: ByteVector): ByteVector = ???

  override def normalize(str: String): String = ???

  /** @param x x coordinate
    * @return a tuple (p1, p2) where p1 and p2 are points on the curve and p1.x = p2.x = x
    *         p1.y is even, p2.y is odd
    */
  override def recoverPoint(x: BigInteger): (ECPoint, ECPoint) = ???

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
      message: ByteVector): (ECPublicKey, ECPublicKey) = ???

  override def publicKey(privateKey: ECPrivateKey): ECPublicKey = ???

  override def sign(
      privateKey: ECPrivateKey,
      dataToSign: ByteVector): ECDigitalSignature = ???

  override def signWithEntropy(
      privateKey: ECPrivateKey,
      bytes: ByteVector,
      entropy: ByteVector): ECDigitalSignature = ???

  override def secKeyVerify(privateKeybytes: ByteVector): Boolean = ???

  override def verify(
      publicKey: ECPublicKey,
      data: ByteVector,
      signature: ECDigitalSignature): Boolean = ???

  override def decompressed(publicKey: ECPublicKey): ECPublicKey = ???

  override def tweakMultiply(
      publicKey: ECPublicKey,
      tweak: FieldElement): ECPublicKey = ???

  override def add(pk1: ECPrivateKey, pk2: ECPrivateKey): ECPrivateKey = ???

  override def add(bytes: ByteVector, pk2: ECPrivateKey): ByteVector = ???

  override def add(pk1: ECPublicKey, pk2: ECPublicKey): ECPublicKey = ???

  override def pubKeyTweakAdd(
      pubkey: ECPublicKey,
      privkey: ECPrivateKey): ECPublicKey = ???

  override def isValidPubKey(bytes: ByteVector): Boolean = ???

  override def isFullyValidWithBouncyCastle(bytes: ByteVector): Boolean = ???

  override def schnorrSign(
      dataToSign: ByteVector,
      privateKey: ECPrivateKey,
      auxRand: ByteVector): SchnorrDigitalSignature = ???

  override def schnorrSignWithNonce(
      dataToSign: ByteVector,
      privateKey: ECPrivateKey,
      nonceKey: ECPrivateKey): SchnorrDigitalSignature = ???

  override def schnorrVerify(
      data: ByteVector,
      schnorrPubKey: SchnorrPublicKey,
      signature: SchnorrDigitalSignature): Boolean = ???

  override def schnorrComputeSigPoint(
      data: ByteVector,
      nonce: SchnorrNonce,
      pubKey: SchnorrPublicKey,
      compressed: Boolean): ECPublicKey = ???

  override def adaptorSign(
      key: ECPrivateKey,
      adaptorPoint: ECPublicKey,
      msg: ByteVector): ECAdaptorSignature = ???

  override def adaptorComplete(
      key: ECPrivateKey,
      adaptorSignature: ECAdaptorSignature): ECDigitalSignature = ???

  override def extractAdaptorSecret(
      signature: ECDigitalSignature,
      adaptorSignature: ECAdaptorSignature,
      key: ECPublicKey): ECPrivateKey = ???

  override def adaptorVerify(
      adaptorSignature: ECAdaptorSignature,
      key: ECPublicKey,
      msg: ByteVector,
      adaptorPoint: ECPublicKey): Boolean = ???
}

object BCryptoCryptoRuntime extends BCryptoCryptoRuntime

/*
class JsCryptoRuntime extends CryptoRuntime {

  implicit def bufferToByteVector(b: Buffer): ByteVector = {
    val it = b.values()
    var e = it.next()
    var arr = scala.Array.empty[Byte]
    while (!e.done) {
      arr = arr :+ e.value.toByte
      e = it.next()
    }
    ByteVector(arr)
  }

  def randomBytes(n: Int): ByteVector = Random.randomBytes(n)

  def randomBytesBrowser(n: Int): ByteVector = RandomBrowser.randomBytes(n)

//  override def generatePrivateKey: BigInteger = {
//    val bytes = randomBytes(32)
//    new BigInteger(1, bytes)
//  }

  override def ripeMd160(bytes: ByteVector): RipeMd160Digest = {
    val ripeMd160 = new RipeMd160
    ripeMd160.init()
    ripeMd160.update(toNodeBuffer(bytes))
    val hashBytes = ripeMd160.`final`()
    val byteVector = toByteVector(buffer = hashBytes, len = 20)
    val hash = RipeMd160Digest.fromBytes(byteVector)
    hash
  }

  override def sha256Hash160(bytes: ByteVector): Sha256Hash160Digest = {
    val h = new Hash160()
    h.init()
    h.update(toNodeBuffer(bytes))
    val hashBytes = h.`final`()
    val byteVector = toByteVector(buffer = hashBytes, len = 20)
    val hash =
      Sha256Hash160Digest.fromBytes(byteVector)
    hash
  }

  override def toPublicKey(privateKey: ECPrivateKey): ECPublicKey = {
    val sha256 = new SHA256
    val ecdsa = new ECDSA("SECP256K1", sha256, sha256, null)
    val privKeyBuffer = toNodeBuffer(privateKey.bytes)
    val pubKeyBuffer =
      ecdsa.publicKeyCreate(key = privKeyBuffer, compressed = true)
    val byteVector = toByteVector(pubKeyBuffer, 33)
    ECPublicKey.fromBytes(byteVector)
  }

  override def sha256(bytes: ByteVector): Sha256Digest = {
    val sha256 = new SHA256
    sha256.init()
    sha256.update(toNodeBuffer(bytes))
    val hashBytes = sha256.`final`()
    val byteVector = toByteVector(buffer = hashBytes, len = 32)
    val hash = Sha256Digest.fromBytes(byteVector)
    hash
  }

  private def toNodeBuffer(byteVector: ByteVector): Buffer = {
    //the implicit used here is this
    //https://github.com/scala-js/scala-js/blob/b5a93bb99a0b0b5044141d4b2871ea260ef17798/library/src/main/scala/scala/scalajs/js/typedarray/package.scala#L33
    Buffer.from(byteVector.toArray.toTypedArray.buffer)
  }

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
}
 */
