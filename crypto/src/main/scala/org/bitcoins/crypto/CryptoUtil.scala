package org.bitcoins.crypto

import scodec.bits.ByteVector

/** Utility cryptographic functions
  * This is a proxy for the underlying implementation of [[CryptoRuntime]]
  * such as [[LibSecp256k1CryptoRuntime]].
  *
  * This is necessary so that the core module doesn't need to be refactored
  * to add support for multiple platforms, it can keep referencing CryptoUtil
  */
trait CryptoUtil extends CryptoRuntime {

  /** The underlying runtime for the specific platform we are running on */
  private lazy val cryptoRuntime: CryptoRuntime = CryptoContext.cryptoRuntime

  override lazy val cryptoContext: CryptoContext = cryptoRuntime.cryptoContext

  override def freshPrivateKey: ECPrivateKey = {
    cryptoRuntime.freshPrivateKey
  }

  override def toPublicKey(privateKey: ECPrivateKey): ECPublicKey = {
    cryptoRuntime.toPublicKey(privateKey)
  }

  override def normalize(str: String): String = {
    cryptoRuntime.normalize(str)
  }

  /** Does the following computation: RIPEMD160(SHA256(hex)). */
  override def sha256Hash160(bytes: ByteVector): Sha256Hash160Digest = {
    cryptoRuntime.sha256Hash160(bytes)
  }

  def sha256Hash160(str: String): Sha256Hash160Digest = {
    cryptoRuntime.sha256Hash160(serializeForHash(str))
  }

  def doubleSHA256(str: String): DoubleSha256Digest = {
    cryptoRuntime.doubleSHA256(serializeForHash(str))
  }

  /** Takes sha256(bytes). */
  override def sha256(bytes: ByteVector): Sha256Digest = {
    cryptoRuntime.sha256(bytes)
  }

  def taggedSha256(str: String, tag: String): Sha256Digest = {
    cryptoRuntime.taggedSha256(serializeForHash(str), tag)
  }

  /** Performs SHA3-256(bytes) */
  override def sha3_256(bytes: ByteVector): Sha3_256Digest = {
    cryptoRuntime.sha3_256(bytes)
  }

  /** Performs SHA1(bytes). */
  override def sha1(bytes: ByteVector): Sha1Digest = {
    cryptoRuntime.sha1(bytes)
  }

  def sha1(str: String): Sha1Digest = {
    cryptoRuntime.sha1(serializeForHash(str))
  }

  /** Performs RIPEMD160(bytes). */
  override def ripeMd160(bytes: ByteVector): RipeMd160Digest = {
    cryptoRuntime.ripeMd160(bytes)
  }

  def ripeMd160(str: String): RipeMd160Digest = {
    cryptoRuntime.ripeMd160(serializeForHash(str))
  }

  /** Calculates `HMAC-SHA512(key, data)`
    */
  override def hmac512(key: ByteVector, data: ByteVector): ByteVector = {
    cryptoRuntime.hmac512(key, data)
  }

  override def recoverPublicKey(
      signature: ECDigitalSignature,
      message: ByteVector): (ECPublicKey, ECPublicKey) = {
    cryptoRuntime.recoverPublicKey(signature, message)
  }

  override def publicKey(privateKey: ECPrivateKey): ECPublicKey =
    cryptoRuntime.publicKey(privateKey)

  override def sign(
      privateKey: ECPrivateKey,
      dataToSign: ByteVector): ECDigitalSignature = {
    val sig = cryptoRuntime.sign(privateKey, dataToSign)
    assert(
      verify(privateKey.publicKey, dataToSign, sig),
      "Something has gone wrong, a generated signature may have been corrupted")
    sig
  }

  override def signWithEntropy(
      privateKey: ECPrivateKey,
      bytes: ByteVector,
      entropy: ByteVector): ECDigitalSignature = {
    val sig = cryptoRuntime.signWithEntropy(privateKey, bytes, entropy)
    assert(
      verify(privateKey.publicKey, bytes, sig),
      "Something has gone wrong, a generated signature may have been corrupted")
    sig
  }

  override def secKeyVerify(privateKeybytes: ByteVector): Boolean =
    cryptoRuntime.secKeyVerify(privateKeybytes)

  override def verify(
      publicKey: PublicKey,
      data: ByteVector,
      signature: ECDigitalSignature): Boolean =
    cryptoRuntime.verify(publicKey, data, signature)

  override def decompressed(pubKeyBytes: ByteVector): ByteVector =
    cryptoRuntime.decompressed(pubKeyBytes)

  override def decompressed[PK <: PublicKey](publicKey: PK): publicKey.type =
    cryptoRuntime.decompressed(publicKey)

  override def tweakMultiply(
      publicKey: ECPublicKey,
      tweak: FieldElement): ECPublicKey =
    cryptoRuntime.tweakMultiply(publicKey, tweak)

  override def add(pk1: ECPrivateKey, pk2: ECPrivateKey): ECPrivateKey =
    cryptoRuntime.add(pk1, pk2)

  override def add(pk1: ByteVector, pk2: ECPrivateKey): ByteVector =
    cryptoRuntime.add(pk1, pk2)

  override def add(point1: SecpPoint, point2: SecpPoint): SecpPoint =
    cryptoRuntime.add(point1, point2)

  override def add(pk1: ECPublicKey, pk2: ECPublicKey): ECPublicKey =
    cryptoRuntime.add(pk1, pk2)

  override def combinePubKeys(pubKeys: Vector[ECPublicKey]): ECPublicKey =
    cryptoRuntime.combinePubKeys(pubKeys)

  override def pubKeyTweakAdd(
      pubkey: ECPublicKey,
      privkey: ECPrivateKey): ECPublicKey =
    cryptoRuntime.pubKeyTweakAdd(pubkey, privkey)

  override def isValidPubKey(pubKey: PublicKey): Boolean =
    cryptoRuntime.isValidPubKey(pubKey)

  override def schnorrSign(
      dataToSign: ByteVector,
      privateKey: ECPrivateKey,
      auxRand: ByteVector): SchnorrDigitalSignature = {
    val sig = cryptoRuntime.schnorrSign(dataToSign, privateKey, auxRand)
    assert(
      schnorrVerify(dataToSign, privateKey.schnorrPublicKey, sig),
      "Something has gone wrong, a generated signature may have been corrupted")
    sig
  }

  override def schnorrSignWithNonce(
      dataToSign: ByteVector,
      privateKey: ECPrivateKey,
      nonceKey: ECPrivateKey): SchnorrDigitalSignature = {
    val sig =
      cryptoRuntime.schnorrSignWithNonce(dataToSign, privateKey, nonceKey)
    assert(
      schnorrVerify(dataToSign, privateKey.schnorrPublicKey, sig),
      "Something has gone wrong, a generated signature may have been corrupted")
    sig
  }

  override def schnorrVerify(
      data: ByteVector,
      schnorrPubKey: SchnorrPublicKey,
      signature: SchnorrDigitalSignature): Boolean =
    cryptoRuntime.schnorrVerify(data, schnorrPubKey, signature)

  override def schnorrComputeSigPoint(
      data: ByteVector,
      nonce: SchnorrNonce,
      pubKey: SchnorrPublicKey,
      compressed: Boolean): ECPublicKey =
    cryptoRuntime.schnorrComputeSigPoint(data, nonce, pubKey, compressed)

  override def adaptorSign(
      key: ECPrivateKey,
      adaptorPoint: ECPublicKey,
      msg: ByteVector,
      auxRand: ByteVector): ECAdaptorSignature =
    cryptoRuntime.adaptorSign(key, adaptorPoint, msg, auxRand)

  override def adaptorComplete(
      key: ECPrivateKey,
      adaptorSignature: ECAdaptorSignature): ECDigitalSignature =
    cryptoRuntime.adaptorComplete(key, adaptorSignature)

  override def extractAdaptorSecret(
      signature: ECDigitalSignature,
      adaptorSignature: ECAdaptorSignature,
      key: ECPublicKey): ECPrivateKey =
    cryptoRuntime.extractAdaptorSecret(signature, adaptorSignature, key)

  override def adaptorVerify(
      adaptorSignature: ECAdaptorSignature,
      key: ECPublicKey,
      msg: ByteVector,
      adaptorPoint: ECPublicKey): Boolean =
    cryptoRuntime.adaptorVerify(adaptorSignature, key, msg, adaptorPoint)

  override def decodeSignature(
      signature: ECDigitalSignature): (BigInt, BigInt) =
    cryptoRuntime.decodeSignature(signature)

  override def isValidSignatureEncoding(
      signature: ECDigitalSignature): Boolean =
    cryptoRuntime.isValidSignatureEncoding(signature)

  override def isDEREncoded(signature: ECDigitalSignature): Boolean =
    cryptoRuntime.isDEREncoded(signature)

  override def sipHash(item: ByteVector, key: SipHashKey): Long =
    cryptoRuntime.sipHash(item, key)

  override def decodePoint(bytes: ByteVector): SecpPoint =
    cryptoRuntime.decodePoint(bytes)

  override def randomBytes(n: Int): ByteVector = cryptoRuntime.randomBytes(n)

  override def pbkdf2WithSha512(
      pass: ByteVector,
      salt: ByteVector,
      iterationCount: Int,
      derivedKeyLength: Int): ByteVector =
    cryptoRuntime.pbkdf2WithSha512(pass, salt, iterationCount, derivedKeyLength)
}

object CryptoUtil extends CryptoUtil
