package org.bitcoins.crypto

import scodec.bits.ByteVector

import java.math.BigInteger
import scala.annotation.tailrec

/** Represents the raw bytes which are meant to represent an ECKey without deserializing. */
sealed trait ECKeyBytes extends NetworkElement

/** Represents a serialization sensitive ECPrivateKey (such as is used in WIF). */
case class ECPrivateKeyBytes(bytes: ByteVector, isCompressed: Boolean = true)
    extends ECKeyBytes
    with MaskedToString {
  val toPrivateKey: ECPrivateKey = ECPrivateKey(bytes)

  /** Returns the raw ECPublicKeyBytes serialized using isCompressed. */
  def publicKeyBytes: ECPublicKeyBytes = {
    val pubKey = toPrivateKey.publicKey
    if (isCompressed) {
      ECPublicKeyBytes(pubKey.bytes)
    } else {
      ECPublicKeyBytes(pubKey.decompressedBytes)
    }
  }

  override def toStringSensitive: String = s"ECPrivateKeyBytes($hex)"
}

object ECPrivateKeyBytes extends Factory[ECPrivateKeyBytes] {

  override def fromBytes(bytes: ByteVector): ECPrivateKeyBytes = {
    val modifiedBytes = ECPrivateKey.fromBytes(bytes).bytes

    new ECPrivateKeyBytes(modifiedBytes)
  }

  def freshPrivateKey(isCompressed: Boolean): ECPrivateKeyBytes = {
    CryptoUtil.freshPrivateKey.toPrivateKeyBytes(isCompressed)
  }

  def freshPrivateKey: ECPrivateKeyBytes = {
    CryptoUtil.freshPrivateKey.toPrivateKeyBytes()
  }
}

/** Represents any type which wraps public key bytes which can be used for ECDSA verification.
  * Should always be instantiated with class X extends PublicKey[X].
  */
sealed trait PublicKey[PK <: PublicKey[PK]] extends NetworkElement {

  /** The fromBytes function for the PK type. */
  private[crypto] def fromBytes(bytes: ByteVector): PK

  private[crypto] def fromHex(hex: String): PK = {
    fromBytes(CryptoBytesUtil.decodeHex(hex))
  }

  /** Returns this but as a PK. */
  private def thisAsPK: PK = {
    this.asInstanceOf[PK]
  }

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

  /** Returns true if the underlying bytes being wrapped are compressed */
  def isCompressed: Boolean = bytes.size == 33

  /** Returns true if the underlying bytes being wrapped are valid according to secp256k1 */
  def isFullyValid: Boolean = ECPublicKey.isFullyValid(bytes)

  /** Returns the decompressed version of this PublicKey */
  lazy val decompressed: PK = {
    if (isCompressed) {
      CryptoUtil.decompressed(thisAsPK)
    } else thisAsPK
  }

  /** Returns the compressed version of this PublicKey */
  lazy val compressed: PK = {
    if (isCompressed || bytes == ByteVector.fromByte(0x00)) {
      thisAsPK
    } else {
      val key = if (bytes.length == 65) this else decompressed
      val (x, y) = key.bytes.tail.splitAt(32)
      val leadByte = if (FieldElement(y).isEven) 2.toByte else 3.toByte
      fromBytes(x.+:(leadByte))
    }
  }
}

/** Wraps raw ECPublicKey bytes without doing any validation or deserialization (may be invalid). */
case class ECPublicKeyBytes(bytes: ByteVector)
    extends ECKeyBytes
    with PublicKey[ECPublicKeyBytes] {

  /** Parse these bytes into the bitcoin-s internal public key type. */
  def toPublicKey: ECPublicKey = ECPublicKey(bytes)

  override private[crypto] def fromBytes(bytes: ByteVector): ECPublicKeyBytes =
    ECPublicKeyBytes(bytes)
}

object ECPublicKeyBytes extends Factory[ECPublicKeyBytes] {

  override def fromBytes(bytes: ByteVector): ECPublicKeyBytes = {
    new ECPublicKeyBytes(bytes)
  }

  def freshPublicKey: ECPublicKeyBytes = {
    ECPrivateKeyBytes.freshPrivateKey.publicKeyBytes
  }
}

/** Created by chris on 2/16/16.
  * Represents a fully parsed and validated ECDSA private or public key.
  */
sealed abstract class BaseECKey extends NetworkElement

/** Created by chris on 2/16/16.
  * A valid deserialized private key.
  *
  * Note that there is no notion of compressed vs. decompressed
  * as there is in Wallet Import Format (WIF), if dealing with
  * external wallets then ECPrivateKeyBytes may be needed.
  */
case class ECPrivateKey(bytes: ByteVector)
    extends BaseECKey
    with Sign
    with MaskedToString {
  require(CryptoUtil.secKeyVerify(bytes), s"Invalid key, hex: ${bytes.toHex}")

  /** Signs a given sequence of bytes with the signingKey
    * @param dataToSign the bytes to be signed
    * @return the digital signature
    */
  override def sign(dataToSign: ByteVector): ECDigitalSignature = {
    CryptoUtil.sign(this, dataToSign)
  }

  def sign(hash: HashDigest): ECDigitalSignature = sign(hash.bytes)

  override def signWithEntropy(
      bytes: ByteVector,
      entropy: ByteVector): ECDigitalSignature = {
    CryptoUtil.signWithEntropy(this, bytes, entropy)
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
    val auxRand = ECPrivateKey.freshPrivateKey.bytes
    adaptorSign(adaptorPoint, msg, auxRand)
  }

  def adaptorSign(
      adaptorPoint: ECPublicKey,
      msg: ByteVector,
      auxRand: ByteVector): ECAdaptorSignature = {
    CryptoUtil.adaptorSign(this, adaptorPoint, msg, auxRand)
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

  override def toStringSensitive: String = s"ECPrivateKey($hex)"

  def toPrivateKeyBytes(isCompressed: Boolean = true): ECPrivateKeyBytes = {
    ECPrivateKeyBytes(bytes, isCompressed)
  }
}

object ECPrivateKey extends Factory[ECPrivateKey] {

  @tailrec
  override def fromBytes(bytes: ByteVector): ECPrivateKey = {
    if (bytes.size == 32)
      new ECPrivateKey(bytes)
    else if (bytes.size < 32) {
      //means we need to pad the private key with 0 bytes so we have 32 bytes
      ECPrivateKey.fromBytes(bytes.padLeft(32))
    } //this is for the case when java serialies a BigInteger to 33 bytes to hold the signed num representation
    else if (bytes.size == 33)
      ECPrivateKey.fromBytes(bytes.slice(1, 33))
    else
      throw new IllegalArgumentException(
        "Private keys cannot be greater than 33 bytes in size, got: " +
          CryptoBytesUtil.encodeHex(bytes) + " which is of size: " + bytes.size)
  }

  def fromFieldElement(fieldElement: FieldElement): ECPrivateKey = {
    fieldElement.toPrivateKey
  }

  /** Generates a fresh [[org.bitcoins.crypto.ECPrivateKey ECPrivateKey]] that has not been used before. */
  def apply(): ECPrivateKey = ECPrivateKey.freshPrivateKey

  /** Generates a fresh [[org.bitcoins.crypto.ECPrivateKey ECPrivateKey]] that has not been used before. */
  def freshPrivateKey: ECPrivateKey = CryptoUtil.freshPrivateKey
}

/** Created by chris on 2/16/16.
  * A valid deserialized ECDSA public key.
  *
  * This class wraps some underlying _bytes but after checking that these _bytes are valid,
  * all serializations (compressed and decompressed) of this public key are (lazily) computed
  * where the decompressed version is used internally for computation and the compressed version
  * is provided by the NetworkElement::bytes member.
  *
  * Note that 0x00 is not a valid ECPublicKey but is a valid SecpPoint meaning that if you are
  * doing computations on public key (points) that may have intermediate 0x00 values, then you
  * should convert using toPoint, do computation, and then convert back toPublicKey in the end.
  */
case class ECPublicKey(private val _bytes: ByteVector)
    extends BaseECKey
    with PublicKey[ECPublicKey] {
  require(isFullyValid, s"Invalid public key: ${_bytes}")

  /** Converts this public key into the raw underlying point on secp256k1 for computation. */
  def toPoint: SecpPointFinite = SecpPoint.fromPublicKey(this)

  override private[crypto] def fromBytes(bytes: ByteVector): ECPublicKey = {
    ECPublicKey.fromBytes(bytes)
  }

  def schnorrVerify(
      data: ByteVector,
      signature: SchnorrDigitalSignature): Boolean = {
    schnorrPublicKey.verify(data, signature)
  }

  def schnorrComputePoint(
      data: ByteVector,
      nonce: SchnorrNonce): ECPublicKey = {
    schnorrPublicKey.computeSigPoint(data, nonce)
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

  /** Returns true only if the underlying wrapped _bytes are compressed */
  override def isCompressed: Boolean = _bytes.size == 33

  /** @inheritdoc */
  override def isFullyValid: Boolean = ECPublicKey.isFullyValid(_bytes)

  /** Returns this same ECPublicKey wrapping the underlying compressed _bytes.
    * This function doesn't really have any use, don't use it probably.
    */
  override lazy val compressed: ECPublicKey = {
    if (isCompressed || _bytes == ByteVector.fromByte(0x00)) {
      this
    } else {
      val key = if (_bytes.length == 65) this else decompressed
      val (x, y) = key._bytes.tail.splitAt(32)
      val leadByte = if (FieldElement(y).isEven) 2.toByte else 3.toByte
      ECPublicKey(x.+:(leadByte))
    }
  }

  /** Returns the compressed representation of this ECPublicKey */
  override def bytes: ByteVector = {
    compressed._bytes
  }

  /** Returns the decompressed representation of this ECPublicKey */
  def decompressedBytes: ByteVector = {
    decompressed._bytes
  }

  def decompressedHex: String = {
    decompressedBytes.toHex
  }

  /** Converts this ECPublicKey to raw ECPublicKeyBytes using the specified serialization. */
  def toPublicKeyBytes(isCompressed: Boolean = true): ECPublicKeyBytes = {
    val bs = if (isCompressed) bytes else decompressedBytes

    ECPublicKeyBytes(bs)
  }

  override def equals(obj: Any): Boolean = {
    obj match {
      case pubKey: ECPublicKey => bytes == pubKey.bytes
      case _                   => false
    }
  }

  /** Adds this ECPublicKey to another as points and returns the resulting ECPublicKey.
    * If you are adding more than two points together use CryptoUtil.combinePubKeys instead.
    */
  def add(otherKey: ECPublicKey): ECPublicKey =
    CryptoUtil.add(this, otherKey)

  def tweakMultiply(tweak: FieldElement): ECPublicKey = {
    CryptoUtil.tweakMultiply(this, tweak)
  }
}

object ECPublicKey extends Factory[ECPublicKey] {

  override def fromBytes(bytes: ByteVector): ECPublicKey = {
    new ECPublicKey(bytes)
  }

  def apply(): ECPublicKey = freshPublicKey

  def apply(point: SecpPointFinite): ECPublicKey = point.toPublicKey

  val dummy: ECPublicKey = FieldElement.one.getPublicKey

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
