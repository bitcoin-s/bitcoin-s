package org.bitcoins.crypto

import scodec.bits.{BitVector, ByteVector}

/** Trait that should be extended by specific runtimes like javascript
  * or the JVM to support crypto functions needed for bitcoin-s
  */
trait CryptoRuntime {

  val cryptoContext: CryptoContext

  /** Generates a 32 byte private key */
  def freshPrivateKey: ECPrivateKey

  /** Converts a private key -> public key
    * @param privateKey the private key we want the corresponding public key for
    * @param isCompressed whether the returned public key should be compressed or not
    */
  def toPublicKey(privateKey: ECPrivateKey, isCompressed: Boolean): ECPublicKey

  def ripeMd160(bytes: ByteVector): RipeMd160Digest

  def sha256Hash160(bytes: ByteVector): Sha256Hash160Digest

  def sha256(bytes: ByteVector): Sha256Digest

  def sha256(str: String): Sha256Digest = {
    sha256(serializeForHash(str))
  }

  def sha256(bitVector: BitVector): Sha256Digest = {
    sha256(bitVector.toByteVector)
  }

  def taggedSha256(bytes: ByteVector, tag: String): Sha256Digest = {
    val tagHash = sha256(tag)
    val tagBytes = tagHash.bytes ++ tagHash.bytes
    sha256(tagBytes ++ bytes)
  }

  /** Performs sha256(sha256(bytes)). */
  def doubleSHA256(bytes: ByteVector): DoubleSha256Digest = {
    val hash: ByteVector = sha256(sha256(bytes).bytes).bytes
    DoubleSha256Digest(hash)
  }

  def sha1(bytes: ByteVector): Sha1Digest

  def hmac512(key: ByteVector, data: ByteVector): ByteVector

  def normalize(str: String): String

  def serializeForHash(str: String): ByteVector = {
    ByteVector(normalize(str).getBytes("UTF-8"))
  }

  // The tag "BIP0340/challenge"
  private lazy val schnorrChallengeTagBytes = {
    ByteVector
      .fromValidHex(
        "7bb52d7a9fef58323eb1bf7a407db382d2f3f2d81bb1224f49fe518f6d48d37c7bb52d7a9fef58323eb1bf7a407db382d2f3f2d81bb1224f49fe518f6d48d37c"
      )
  }

  // The tag "DLC/oracle/attestation/v0"
  private val dlcAttestationTagBytes = {
    ByteVector
      .fromValidHex(
        "0c2fa46216e6e460e5e3f78555b102c5ac6aecabbfb82b430cf36cdfe04421790c2fa46216e6e460e5e3f78555b102c5ac6aecabbfb82b430cf36cdfe0442179"
      )
  }

  def sha256SchnorrChallenge(bytes: ByteVector): Sha256Digest = {
    sha256(schnorrChallengeTagBytes ++ bytes)
  }

  def sha256DLCAttestation(bytes: ByteVector): Sha256Digest = {
    sha256(dlcAttestationTagBytes ++ bytes)
  }

  def sha256DLCAttestation(str: String): Sha256Digest = {
    sha256DLCAttestation(serializeForHash(str))
  }

  // The tag "DLC/oracle/announcement/v0"
  private val dlcAnnouncementTagBytes = {
    ByteVector
      .fromValidHex(
        "6378871e8c99d480fff016e178a371e7e058445eff3023fe158f05aa185ed0e16378871e8c99d480fff016e178a371e7e058445eff3023fe158f05aa185ed0e1"
      )
  }

  def sha256DLCAnnouncement(bytes: ByteVector): Sha256Digest = {
    sha256(dlcAnnouncementTagBytes ++ bytes)
  }

  /** Recover public keys from a signature and the message that was signed. This method will return 2 public keys, and the signature
    * can be verified with both, but only one of them matches that private key that was used to generate the signature.
    *
    * @param signature       signature
    * @param message message that was signed
    * @return a (pub1, pub2) tuple where pub1 and pub2 are candidates public keys. If you have the recovery id  then use
    *         pub1 if the recovery id is even and pub2 if it is odd
    */
  def recoverPublicKey(
      signature: ECDigitalSignature,
      message: ByteVector): (ECPublicKey, ECPublicKey)

  // The tag "BIP0340/aux"
  private val schnorrAuxTagBytes = {
    ByteVector
      .fromValidHex(
        "f1ef4e5ec063cada6d94cafa9d987ea069265839ecc11f972d77a52ed8c1cc90f1ef4e5ec063cada6d94cafa9d987ea069265839ecc11f972d77a52ed8c1cc90"
      )
  }

  def sha256SchnorrAuxRand(bytes: ByteVector): Sha256Digest = {
    sha256(schnorrAuxTagBytes ++ bytes)
  }

  // The tag "BIP0340/nonce"
  private val schnorrNonceTagBytes = {
    ByteVector
      .fromValidHex(
        "07497734a79bcb355b9b8c7d034f121cf434d73ef72dda19870061fb52bfeb2f07497734a79bcb355b9b8c7d034f121cf434d73ef72dda19870061fb52bfeb2f"
      )
  }

  def sha256SchnorrNonce(bytes: ByteVector): Sha256Digest = {
    sha256(schnorrNonceTagBytes ++ bytes)
  }

  def publicKey(privateKey: ECPrivateKey): ECPublicKey

  def sign(privateKey: ECPrivateKey, dataToSign: ByteVector): ECDigitalSignature

  def signWithEntropy(
      privateKey: ECPrivateKey,
      bytes: ByteVector,
      entropy: ByteVector): ECDigitalSignature

  def secKeyVerify(privateKeybytes: ByteVector): Boolean

  def verify(
      publicKey: ECPublicKey,
      data: ByteVector,
      signature: ECDigitalSignature): Boolean

  def decompressed(publicKey: ECPublicKey): ECPublicKey

  def tweakMultiply(publicKey: ECPublicKey, tweak: FieldElement): ECPublicKey

  def add(pk1: ECPrivateKey, pk2: ECPrivateKey): ECPrivateKey

  def add(bytes: ByteVector, pk2: ECPrivateKey): ByteVector

  def add(pk1: ECPublicKey, pk2: ECPublicKey): ECPublicKey

  def pubKeyTweakAdd(pubkey: ECPublicKey, privkey: ECPrivateKey): ECPublicKey

  def isValidPubKey(bytes: ByteVector): Boolean

  def isFullyValidWithBouncyCastle(bytes: ByteVector): Boolean

  def schnorrSign(
      dataToSign: ByteVector,
      privateKey: ECPrivateKey,
      auxRand: ByteVector): SchnorrDigitalSignature

  def schnorrSignWithNonce(
      dataToSign: ByteVector,
      privateKey: ECPrivateKey,
      nonceKey: ECPrivateKey): SchnorrDigitalSignature

  def schnorrVerify(
      data: ByteVector,
      schnorrPubKey: SchnorrPublicKey,
      signature: SchnorrDigitalSignature): Boolean

  def schnorrComputeSigPoint(
      data: ByteVector,
      nonce: SchnorrNonce,
      pubKey: SchnorrPublicKey,
      compressed: Boolean): ECPublicKey

  def adaptorSign(
      key: ECPrivateKey,
      adaptorPoint: ECPublicKey,
      msg: ByteVector): ECAdaptorSignature

  def adaptorComplete(
      key: ECPrivateKey,
      adaptorSignature: ECAdaptorSignature): ECDigitalSignature

  def extractAdaptorSecret(
      signature: ECDigitalSignature,
      adaptorSignature: ECAdaptorSignature,
      key: ECPublicKey): ECPrivateKey

  def adaptorVerify(
      adaptorSignature: ECAdaptorSignature,
      key: ECPublicKey,
      msg: ByteVector,
      adaptorPoint: ECPublicKey): Boolean

  def decodeSignature(signature: ECDigitalSignature): (BigInt, BigInt)

  def isValidSignatureEncoding(signature: ECDigitalSignature): Boolean

  def isDEREncoded(signature: ECDigitalSignature): Boolean

  /** https://github.com/bitcoin/bips/blob/master/bip-0158.mediawiki#hashing-data-objects */
  def sipHash(item: ByteVector, key: SipHashKey): Long
}
