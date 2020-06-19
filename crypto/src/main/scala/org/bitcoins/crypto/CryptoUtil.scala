package org.bitcoins.crypto

import java.math.BigInteger
import java.security.MessageDigest

import org.bouncycastle.crypto.digests.{RIPEMD160Digest, SHA512Digest}
import org.bouncycastle.crypto.macs.HMac
import org.bouncycastle.crypto.params.KeyParameter
import org.bouncycastle.math.ec.ECPoint
import scodec.bits.{BitVector, ByteVector}

/**
  * Utility cryptographic functions
  */
trait CryptoUtil {

  /** Does the following computation: RIPEMD160(SHA256(hex)). */
  def sha256Hash160(bytes: ByteVector): Sha256Hash160Digest = {
    val hash = ripeMd160(sha256(bytes).bytes).bytes
    Sha256Hash160Digest(hash)
  }

  /** Performs sha256(sha256(bytes)). */
  def doubleSHA256(bytes: ByteVector): DoubleSha256Digest = {
    val hash: ByteVector = sha256(sha256(bytes).bytes).bytes
    DoubleSha256Digest(hash)
  }

  /** Takes sha256(bytes). */
  def sha256(bytes: ByteVector): Sha256Digest = {
    val hash = MessageDigest.getInstance("SHA-256").digest(bytes.toArray)
    Sha256Digest(ByteVector(hash))
  }

  /** Takes sha256(bits). */
  def sha256(bits: BitVector): Sha256Digest = {
    sha256(bits.toByteVector)
  }

  def taggedSha256(bytes: ByteVector, tag: String): Sha256Digest = {
    val tagHash = sha256(ByteVector(tag.getBytes()))
    val tagBytes = tagHash.bytes ++ tagHash.bytes
    sha256(tagBytes ++ bytes)
  }

  // The tag "BIP340/challenge"
  private val schnorrChallengeTagBytes = {
    ByteVector
      .fromValidHex(
        "07e00dcd3055c1b36ee93effe4d7f266024cdef4116982ff5dfdc1a97e77062907e00dcd3055c1b36ee93effe4d7f266024cdef4116982ff5dfdc1a97e770629"
      )
  }

  def sha256SchnorrChallenge(bytes: ByteVector): Sha256Digest = {
    sha256(schnorrChallengeTagBytes ++ bytes)
  }

  // The tag "BIP340/nonce"
  private val schnorrNonceTagBytes = {
    ByteVector
      .fromValidHex(
        "a2ba14a6b39c1c505260bf3aceb07febde3ab34c35c9259d25bd6972f15e6564a2ba14a6b39c1c505260bf3aceb07febde3ab34c35c9259d25bd6972f15e6564"
      )
  }

  def sha256SchnorrNonce(bytes: ByteVector): Sha256Digest = {
    sha256(schnorrNonceTagBytes ++ bytes)
  }

  // The tag "BIP340/aux"
  private val schnorrAuxTagBytes = {
    ByteVector
      .fromValidHex(
        "4b07426ad8630dcdbadf8dee1e94f09ac2df4e7ee2629e5e6b27c8666c8cf31e4b07426ad8630dcdbadf8dee1e94f09ac2df4e7ee2629e5e6b27c8666c8cf31e"
      )
  }

  def sha256SchnorrAuxRand(bytes: ByteVector): Sha256Digest = {
    sha256(schnorrAuxTagBytes ++ bytes)
  }

  /** Performs SHA1(bytes). */
  def sha1(bytes: ByteVector): Sha1Digest = {
    val hash = MessageDigest.getInstance("SHA-1").digest(bytes.toArray).toList
    Sha1Digest(ByteVector(hash))
  }

  /** Performs RIPEMD160(bytes). */
  def ripeMd160(bytes: ByteVector): RipeMd160Digest = {
    //from this tutorial http://rosettacode.org/wiki/RIPEMD-160#Scala
    val messageDigest = new RIPEMD160Digest
    val raw = bytes.toArray
    messageDigest.update(raw, 0, raw.length)
    val out = Array.fill[Byte](messageDigest.getDigestSize)(0)
    messageDigest.doFinal(out, 0)
    RipeMd160Digest(ByteVector(out))
  }

  /**
    * Calculates `HMAC-SHA512(key, data)`
    */
  def hmac512(key: ByteVector, data: ByteVector): ByteVector = {
    val hmac512 = new HMac(new SHA512Digest())
    hmac512.init(new KeyParameter(key.toArray))
    hmac512.update(data.toArray, 0, data.intSize.get)
    val output = new Array[Byte](64)
    hmac512.doFinal(output, 0)
    ByteVector(output)
  }

  /**
    * @param x x coordinate
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

    (ECPublicKey(0x02.toByte +: bytes32).toPoint,
     ECPublicKey(0x03.toByte +: bytes32).toPoint)
  }

  /**
    * Recover public keys from a signature and the message that was signed. This method will return 2 public keys, and the signature
    * can be verified with both, but only one of them matches that private key that was used to generate the signature.
    *
    * @param signature       signature
    * @param message message that was signed
    * @return a (pub1, pub2) tuple where pub1 and pub2 are candidates public keys. If you have the recovery id  then use
    *         pub1 if the recovery id is even and pub2 if it is odd
    */
  def recoverPublicKey(
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
}

object CryptoUtil extends CryptoUtil
