package org.bitcoins.crypto

import scodec.bits.ByteVector

/** This is an implementation of [[CryptoRuntime]] that defaults to
  * Bcrypto (https://github.com/bcoin-org/bcrypto) when possible.
  */
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

  override def decodeSignature(
      signature: ECDigitalSignature): (scala.BigInt, scala.BigInt) = ???

  override def isValidSignatureEncoding(
      signature: ECDigitalSignature): Boolean = ???

  override def isDEREncoded(signature: ECDigitalSignature): Boolean = ???

  override def sipHash(item: ByteVector, key: SipHashKey): Long = ???
}

object BCryptoCryptoRuntime extends BCryptoCryptoRuntime
