package org.bitcoins.crypto

import scodec.bits.ByteVector

import java.math.BigInteger
import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits
import scala.concurrent.{ExecutionContext, Future}

/** Created by chris on 2/16/16.
  */
sealed abstract class BaseECKey extends NetworkElement

/** Created by chris on 2/16/16.
  */
sealed abstract class ECPrivateKey
    extends BaseECKey
    with Sign
    with MaskedToString {

  override def signFunction: ByteVector => Future[ECDigitalSignature] = {
    bytes =>
      Future.successful(sign(bytes))
  }

  /** Signs a given sequence of bytes with the signingKey
    * @param dataToSign the bytes to be signed
    * @return the digital signature
    */
  override def sign(dataToSign: ByteVector): ECDigitalSignature = {
    CryptoUtil.sign(this, dataToSign)
  }

  def sign(hash: HashDigest): ECDigitalSignature = sign(hash.bytes)

  def signFuture(hash: HashDigest)(implicit
      ec: ExecutionContext): Future[ECDigitalSignature] =
    Future(sign(hash))

  override def signWithEntropy(
      bytes: ByteVector,
      entropy: ByteVector): ECDigitalSignature = {
    CryptoUtil.signWithEntropy(this, bytes, entropy)
  }

  override def signWithEntropyFunction: (
      ByteVector,
      ByteVector) => Future[ECDigitalSignature] = { case (bytes, entropy) =>
    import scala.concurrent.ExecutionContext.Implicits.global
    Future(signWithEntropy(bytes, entropy))
  }

  def schnorrSign(dataToSign: ByteVector): SchnorrDigitalSignature = {
    val auxRand = ECPrivateKey.freshPrivateKey.bytes
    schnorrSign(dataToSign, auxRand)
  }

  def schnorrSign(
      dataToSign: ByteVector,
      auxRand: ByteVector): SchnorrDigitalSignature = {
    CryptoUtil.schnorrSign(dataToSign, this, auxRand)
  }

  def schnorrSignWithNonce(
      dataToSign: ByteVector,
      nonce: ECPrivateKey): SchnorrDigitalSignature = {
    CryptoUtil.schnorrSignWithNonce(dataToSign, this, nonce)
  }

  def adaptorSign(
      adaptorPoint: ECPublicKey,
      msg: ByteVector): ECAdaptorSignature = {
    CryptoUtil.adaptorSign(this, adaptorPoint, msg)
  }

  def completeAdaptorSignature(
      adaptorSignature: ECAdaptorSignature): ECDigitalSignature = {
    CryptoUtil.adaptorComplete(this, adaptorSignature)
  }

  def completeAdaptorSignature(
      adaptorSignature: ECAdaptorSignature,
      hashTypeByte: Byte): ECDigitalSignature = {
    val completedSig = completeAdaptorSignature(adaptorSignature)
    ECDigitalSignature(completedSig.bytes ++ ByteVector.fromByte(hashTypeByte))
  }

  def nonceKey: ECPrivateKey = {
    if (schnorrNonce.publicKey == publicKey) {
      this
    } else {
      this.negate
    }
  }

  def schnorrKey: ECPrivateKey = {
    if (schnorrPublicKey.publicKey == publicKey) {
      this
    } else {
      this.negate
    }
  }

  // CryptoParams.curve.getN
  private val N: BigInteger = new BigInteger(
    "115792089237316195423570985008687907852837564279074904382605163141518161494337")

  def negate: ECPrivateKey = {
    val negPrivKeyNum = N.subtract(new BigInteger(1, bytes.toArray))
    ECPrivateKey(ByteVector(negPrivKeyNum.toByteArray))
  }

  def add(other: ECPrivateKey): ECPrivateKey = {
    CryptoUtil.add(this, other)
  }

  /** Signifies if the this private key corresponds to a compressed public key */
  def isCompressed: Boolean

  /** Derives the public for a the private key */
  override def publicKey: ECPublicKey =
    CryptoUtil.publicKey(this)

  def schnorrPublicKey: SchnorrPublicKey = {
    SchnorrPublicKey(publicKey.bytes)
  }

  def schnorrNonce: SchnorrNonce = {
    SchnorrNonce(publicKey.bytes)
  }

  def fieldElement: FieldElement = FieldElement(bytes)

  override def toStringSensitive: String = s"ECPrivateKey($hex,$isCompressed)"
}

object ECPrivateKey extends Factory[ECPrivateKey] {

  private case class ECPrivateKeyImpl(
      override val bytes: ByteVector,
      isCompressed: Boolean,
      ec: ExecutionContext)
      extends ECPrivateKey {
    require(CryptoUtil.secKeyVerify(bytes), s"Invalid key, hex: ${bytes.toHex}")
  }

  def apply(bytes: ByteVector, isCompressed: Boolean)(implicit
      ec: ExecutionContext): ECPrivateKey = {
    ECPrivateKeyImpl(bytes, isCompressed, ec)
  }

  override def fromBytes(bytes: ByteVector): ECPrivateKey =
    fromBytes(bytes, isCompressed = true)

  @tailrec
  def fromBytes(bytes: ByteVector, isCompressed: Boolean): ECPrivateKey = {

    if (bytes.size == 32)
      ECPrivateKeyImpl(bytes, isCompressed, Implicits.global)
    else if (bytes.size < 32) {
      //means we need to pad the private key with 0 bytes so we have 32 bytes
      ECPrivateKey.fromBytes(bytes.padLeft(32), isCompressed)
    } //this is for the case when java serialies a BigInteger to 33 bytes to hold the signed num representation
    else if (bytes.size == 33)
      ECPrivateKey.fromBytes(bytes.slice(1, 33), isCompressed)
    else
      throw new IllegalArgumentException(
        "Private keys cannot be greater than 33 bytes in size, got: " +
          CryptoBytesUtil.encodeHex(bytes) + " which is of size: " + bytes.size)
  }

  def fromHex(hex: String, isCompressed: Boolean): ECPrivateKey =
    fromBytes(CryptoBytesUtil.decodeHex(hex), isCompressed)

  def fromFieldElement(fieldElement: FieldElement): ECPrivateKey = {
    fieldElement.toPrivateKey
  }

  /** Generates a fresh [[org.bitcoins.crypto.ECPrivateKey ECPrivateKey]] that has not been used before. */
  def apply(): ECPrivateKey = ECPrivateKey(true)

  def apply(isCompressed: Boolean): ECPrivateKey = freshPrivateKey(isCompressed)

  /** Generates a fresh [[org.bitcoins.crypto.ECPrivateKey ECPrivateKey]] that has not been used before. */
  def freshPrivateKey: ECPrivateKey = freshPrivateKey(true)

  def freshPrivateKey(isCompressed: Boolean): ECPrivateKey = {
    val priv = CryptoUtil.freshPrivateKey
    ECPrivateKey.fromBytes(priv.bytes, isCompressed)
  }
}

/** Created by chris on 2/16/16.
  */
sealed abstract class ECPublicKey extends BaseECKey {

  def verify(hash: HashDigest, signature: ECDigitalSignature): Boolean =
    verify(hash.bytes, signature)

  /** Verifies if a given piece of data is signed by the
    * [[org.bitcoins.crypto.ECPrivateKey ECPrivateKey]]'s corresponding
    * [[org.bitcoins.crypto.ECPublicKey ECPublicKey]].
    */
  def verify(data: ByteVector, signature: ECDigitalSignature): Boolean = {
    CryptoUtil.verify(this, data, signature)
  }

  def verify(hex: String, signature: ECDigitalSignature): Boolean =
    verify(CryptoBytesUtil.decodeHex(hex), signature)

  def schnorrVerify(
      data: ByteVector,
      signature: SchnorrDigitalSignature): Boolean = {
    schnorrPublicKey.verify(data, signature)
  }

  def schnorrComputePoint(
      data: ByteVector,
      nonce: SchnorrNonce,
      compressed: Boolean = isCompressed): ECPublicKey = {
    schnorrPublicKey.computeSigPoint(data, nonce, compressed)
  }

  def schnorrPublicKey: SchnorrPublicKey = SchnorrPublicKey(bytes)

  def schnorrNonce: SchnorrNonce = SchnorrNonce(bytes)

  def adaptorVerify(
      msg: ByteVector,
      adaptorPoint: ECPublicKey,
      adaptorSignature: ECAdaptorSignature): Boolean = {
    CryptoUtil.adaptorVerify(adaptorSignature, this, msg, adaptorPoint)
  }

  def extractAdaptorSecret(
      adaptorSignature: ECAdaptorSignature,
      signature: ECDigitalSignature): ECPrivateKey = {
    CryptoUtil.extractAdaptorSecret(signature, adaptorSignature, this)
  }

  override def toString: String = "ECPublicKey(" + hex + ")"

  /** Checks if the [[org.bitcoins.crypto.ECPublicKey ECPublicKey]] is compressed */
  def isCompressed: Boolean = bytes.size == 33

  /** Checks if the [[org.bitcoins.crypto.ECPublicKey ECPublicKey]] is valid according to secp256k1 */
  def isFullyValid: Boolean = ECPublicKey.isFullyValid(bytes)

  /** Returns the decompressed version of this [[org.bitcoins.crypto.ECPublicKey ECPublicKey]] */
  def decompressed: ECPublicKey =
    CryptoUtil.decompressed(this)

  /** Adds this ECPublicKey to another as points and returns the resulting ECPublicKey.
    *
    * Note: if this ever becomes a bottleneck, secp256k1_ec_pubkey_combine should
    * get wrapped in NativeSecp256k1 to speed things up.
    */
  def add(otherKey: ECPublicKey): ECPublicKey =
    CryptoUtil.add(this, otherKey)

  def tweakMultiply(tweak: FieldElement): ECPublicKey = {
    CryptoUtil.tweakMultiply(this, tweak)
  }
}

object ECPublicKey extends Factory[ECPublicKey] {

  private case class ECPublicKeyImpl(
      override val bytes: ByteVector,
      ec: ExecutionContext)
      extends ECPublicKey {
    //unfortunately we cannot place ANY invariants here
    //because of old transactions on the blockchain that have weirdly formatted public keys. Look at example in script_tests.json
    //https://github.com/bitcoin/bitcoin/blob/master/src/test/data/script_tests.json#L457
    //bitcoin core only checks CPubKey::IsValid()
    //this means we can have public keys with only one byte i.e. 0x00 or no bytes.
    //Eventually we would like this to be CPubKey::IsFullyValid() but since we are remaining backwards compatible
    //we cannot do this. If there ever is a hard fork this would be a good thing to add.
  }

  override def fromBytes(bytes: ByteVector): ECPublicKey = {
    ECPublicKeyImpl(bytes, Implicits.global)
  }

  def apply(): ECPublicKey = freshPublicKey

  val dummy: ECPublicKey = FieldElement.one.getPublicKey

  val infinity: ECPublicKey = ECPublicKey.fromBytes(ByteVector(0x00))

  /** Generates a fresh [[org.bitcoins.crypto.ECPublicKey ECPublicKey]] that has not been used before. */
  def freshPublicKey: ECPublicKey = ECPrivateKey.freshPrivateKey.publicKey

  /** Checks if the public key is valid according to secp256k1
    * Mimics this function in bitcoin core
    * [[https://github.com/bitcoin/bitcoin/blob/27765b6403cece54320374b37afb01a0cfe571c3/src/pubkey.cpp#L207-L212]]
    */
  def isFullyValid(bytes: ByteVector): Boolean =
    isValid(bytes) && CryptoUtil.isValidPubKey(bytes)

  /** Mimics the CPubKey::IsValid function in Bitcoin core, this is a consensus rule
    * [[https://github.com/bitcoin/bitcoin/blob/27765b6403cece54320374b37afb01a0cfe571c3/src/pubkey.h#L158]]
    */
  def isValid(bytes: ByteVector): Boolean = bytes.nonEmpty
}
