package org.bitcoins.crypto

import java.math.BigInteger
import java.security.SecureRandom

import org.bitcoin.NativeSecp256k1
import org.bouncycastle.crypto.AsymmetricCipherKeyPair
import org.bouncycastle.crypto.generators.ECKeyPairGenerator
import org.bouncycastle.crypto.params.{
  ECKeyGenerationParameters,
  ECPrivateKeyParameters
}
import org.bouncycastle.math.ec.ECPoint
import scodec.bits.ByteVector

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits
import scala.concurrent.{ExecutionContext, Future}

/**
  * Created by chris on 2/16/16.
  */
sealed abstract class BaseECKey extends NetworkElement

/**
  * Created by chris on 2/16/16.
  */
sealed abstract class ECPrivateKey
    extends BaseECKey
    with Sign
    with MaskedToString {

  override def signFunction: ByteVector => Future[ECDigitalSignature] = {
    bytes =>
      import scala.concurrent.ExecutionContext.Implicits.global
      Future(sign(bytes))
  }

  /**
    * Signs a given sequence of bytes with the signingKey
    * @param dataToSign the bytes to be signed
    * @return the digital signature
    */
  override def sign(dataToSign: ByteVector): ECDigitalSignature = {
    sign(dataToSign, CryptoContext.default)
  }

  def sign(
      dataToSign: ByteVector,
      context: CryptoContext): ECDigitalSignature = {
    require(dataToSign.length == 32 && bytes.length <= 32)
    context match {
      case CryptoContext.LibSecp256k1 => signWithSecp(dataToSign)
      case CryptoContext.BouncyCastle => signWithBouncyCastle(dataToSign)
    }
  }

  def signWithSecp(dataToSign: ByteVector): ECDigitalSignature = {
    val signature =
      NativeSecp256k1.sign(dataToSign.toArray, bytes.toArray)
    ECDigitalSignature(ByteVector(signature))
  }

  def signWithBouncyCastle(dataToSign: ByteVector): ECDigitalSignature = {
    BouncyCastleUtil.sign(dataToSign, this)
  }

  def sign(hash: HashDigest): ECDigitalSignature = sign(hash.bytes)

  def signFuture(hash: HashDigest)(implicit
      ec: ExecutionContext): Future[ECDigitalSignature] =
    Future(sign(hash))

  def schnorrSign(dataToSign: ByteVector): SchnorrDigitalSignature = {
    schnorrSign(dataToSign, CryptoContext.default)
  }

  def schnorrSign(
      dataToSign: ByteVector,
      context: CryptoContext): SchnorrDigitalSignature = {
    val auxRand = ECPrivateKey.freshPrivateKey.bytes
    schnorrSign(dataToSign, auxRand, context)
  }

  def schnorrSign(
      dataToSign: ByteVector,
      auxRand: ByteVector): SchnorrDigitalSignature = {
    schnorrSign(dataToSign, auxRand, CryptoContext.default)
  }

  def schnorrSign(
      dataToSign: ByteVector,
      auxRand: ByteVector,
      context: CryptoContext): SchnorrDigitalSignature = {
    context match {
      case CryptoContext.LibSecp256k1 =>
        schnorrSignWithSecp(dataToSign, auxRand)
      case CryptoContext.BouncyCastle =>
        schnorrSignWithBouncyCastle(dataToSign, auxRand)
    }
  }

  def schnorrSignWithSecp(
      dataToSign: ByteVector,
      auxRand: ByteVector): SchnorrDigitalSignature = {
    val sigBytes =
      NativeSecp256k1.schnorrSign(dataToSign.toArray,
                                  bytes.toArray,
                                  auxRand.toArray)
    SchnorrDigitalSignature(ByteVector(sigBytes))
  }

  def schnorrSignWithBouncyCastle(
      dataToSign: ByteVector,
      auxRand: ByteVector): SchnorrDigitalSignature = {
    BouncyCastleUtil.schnorrSign(dataToSign, this, auxRand)
  }

  def schnorrSignWithNonce(
      dataToSign: ByteVector,
      nonce: ECPrivateKey): SchnorrDigitalSignature = {
    schnorrSignWithNonce(dataToSign, nonce, CryptoContext.default)
  }

  def schnorrSignWithNonce(
      dataToSign: ByteVector,
      nonce: ECPrivateKey,
      context: CryptoContext): SchnorrDigitalSignature = {
    context match {
      case CryptoContext.LibSecp256k1 =>
        schnorrSignWithNonceWithSecp(dataToSign, nonce)
      case CryptoContext.BouncyCastle =>
        schnorrSignWithNonceWithBouncyCastle(dataToSign, nonce)
    }
  }

  def schnorrSignWithNonceWithSecp(
      dataToSign: ByteVector,
      nonce: ECPrivateKey): SchnorrDigitalSignature = {
    val sigBytes =
      NativeSecp256k1.schnorrSignWithNonce(dataToSign.toArray,
                                           bytes.toArray,
                                           nonce.bytes.toArray)
    SchnorrDigitalSignature(ByteVector(sigBytes))
  }

  def schnorrSignWithNonceWithBouncyCastle(
      dataToSign: ByteVector,
      nonce: ECPrivateKey): SchnorrDigitalSignature = {
    BouncyCastleUtil.schnorrSignWithNonce(dataToSign, this, nonce)
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

  def negate: ECPrivateKey = {
    val negPrivKeyNum = CryptoParams.curve.getN
      .subtract(new BigInteger(1, bytes.toArray))
    ECPrivateKey(ByteVector(negPrivKeyNum.toByteArray))
  }

  def add(other: ECPrivateKey): ECPrivateKey = {
    add(other, CryptoContext.default)
  }

  def add(other: ECPrivateKey, context: CryptoContext): ECPrivateKey = {
    context match {
      case CryptoContext.LibSecp256k1 => addWithSecp(other)
      case CryptoContext.BouncyCastle => addWithBouncyCastle(other)
    }
  }

  def addWithSecp(other: ECPrivateKey): ECPrivateKey = {
    val sumBytes =
      NativeSecp256k1.privKeyTweakAdd(bytes.toArray, other.bytes.toArray)
    ECPrivateKey(ByteVector(sumBytes))
  }

  def addWithBouncyCastle(other: ECPrivateKey): ECPrivateKey = {
    fieldElement.add(other.fieldElement).toPrivateKey
  }

  /** Signifies if the this private key corresponds to a compressed public key */
  def isCompressed: Boolean

  override def publicKey: ECPublicKey = publicKey(CryptoContext.default)

  /** Derives the public for a the private key */
  def publicKey(context: CryptoContext): ECPublicKey = {
    context match {
      case CryptoContext.LibSecp256k1 => publicKeyWithSecp
      case CryptoContext.BouncyCastle => publicKeyWithBouncyCastle
    }
  }

  def publicKeyWithSecp: ECPublicKey = {
    val pubKeyBytes: Array[Byte] =
      NativeSecp256k1.computePubkey(bytes.toArray, isCompressed)
    val pubBytes = ByteVector(pubKeyBytes)
    require(
      ECPublicKey.isFullyValid(pubBytes),
      s"secp256k1 failed to generate a valid public key, got: ${CryptoBytesUtil
        .encodeHex(pubBytes)}")
    ECPublicKey(pubBytes)
  }

  def publicKeyWithBouncyCastle: ECPublicKey = {
    BouncyCastleUtil.computePublicKey(this)
  }

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
    CryptoContext.default match {
      case CryptoContext.LibSecp256k1 =>
        require(NativeSecp256k1.secKeyVerify(bytes.toArray),
                s"Invalid key according to secp256k1, hex: ${bytes.toHex}")
      case CryptoContext.BouncyCastle =>
        require(CryptoParams.curve.getCurve
                  .isValidFieldElement(new BigInteger(1, bytes.toArray)),
                s"Invalid key according to Bouncy Castle, hex: ${bytes.toHex}")
    }
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
    val secureRandom = new SecureRandom
    val generator: ECKeyPairGenerator = new ECKeyPairGenerator
    val keyGenParams: ECKeyGenerationParameters =
      new ECKeyGenerationParameters(CryptoParams.curve, secureRandom)
    generator.init(keyGenParams)
    val keypair: AsymmetricCipherKeyPair = generator.generateKeyPair
    val privParams: ECPrivateKeyParameters =
      keypair.getPrivate.asInstanceOf[ECPrivateKeyParameters]
    val priv: BigInteger = privParams.getD
    val bytes = ByteVector(priv.toByteArray)
    ECPrivateKey.fromBytes(bytes, isCompressed)
  }
}

/**
  * Created by chris on 2/16/16.
  */
sealed abstract class ECPublicKey extends BaseECKey {

  def verify(hash: HashDigest, signature: ECDigitalSignature): Boolean =
    verify(hash.bytes, signature)

  /** Verifies if a given piece of data is signed by the
    * [[org.bitcoins.crypto.ECPrivateKey ECPrivateKey]]'s corresponding
    * [[org.bitcoins.crypto.ECPublicKey ECPublicKey]]. */
  def verify(data: ByteVector, signature: ECDigitalSignature): Boolean = {
    verify(data, signature, CryptoContext.default)
  }

  def verify(
      data: ByteVector,
      signature: ECDigitalSignature,
      context: CryptoContext): Boolean = {
    context match {
      case CryptoContext.LibSecp256k1 => verifyWithSecp(data, signature)
      case CryptoContext.BouncyCastle => verifyWithBouncyCastle(data, signature)
    }
  }

  def verifyWithSecp(
      data: ByteVector,
      signature: ECDigitalSignature): Boolean = {
    val result =
      NativeSecp256k1.verify(data.toArray,
                             signature.bytes.toArray,
                             bytes.toArray)

    if (!result) {
      //if signature verification fails with libsecp256k1 we need to use our old
      //verification function from spongy castle, this is needed because early blockchain
      //transactions can have weird non strict der encoded digital signatures
      //bitcoin core implements this functionality here:
      //https://github.com/bitcoin/bitcoin/blob/master/src/pubkey.cpp#L16-L165
      verifyWithBouncyCastle(data, signature)
    } else result
  }

  def verifyWithBouncyCastle(
      data: ByteVector,
      signature: ECDigitalSignature): Boolean = {
    BouncyCastleUtil.verifyDigitalSignature(data, this, signature)
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

  override def toString: String = "ECPublicKey(" + hex + ")"

  /** Checks if the [[org.bitcoins.crypto.ECPublicKey ECPublicKey]] is compressed */
  def isCompressed: Boolean = bytes.size == 33

  /** Checks if the [[org.bitcoins.crypto.ECPublicKey ECPublicKey]] is valid according to secp256k1 */
  def isFullyValid: Boolean = ECPublicKey.isFullyValid(bytes)

  /** Returns the decompressed version of this [[org.bitcoins.crypto.ECPublicKey ECPublicKey]] */
  def decompressed: ECPublicKey = decompressed(CryptoContext.default)

  def decompressed(context: CryptoContext): ECPublicKey = {
    context match {
      case CryptoContext.LibSecp256k1 => decompressedWithSecp
      case CryptoContext.BouncyCastle => decompressedWithBouncyCastle
    }
  }

  def decompressedWithSecp: ECPublicKey = {
    if (isCompressed) {
      val decompressed = NativeSecp256k1.decompress(bytes.toArray)
      ECPublicKey.fromBytes(ByteVector(decompressed))
    } else this
  }

  def decompressedWithBouncyCastle: ECPublicKey = {
    BouncyCastleUtil.decompressPublicKey(this)
  }

  /** Decodes a [[org.bitcoins.crypto.ECPublicKey ECPublicKey]] in bitcoin-s
    * to a [[org.bouncycastle.math.ec.ECPoint ECPoint]] data structure that is internal to the
    * bouncy castle library
    * @return
    */
  def toPoint: ECPoint = {
    BouncyCastleUtil.decodePoint(bytes)
  }

  /** Adds this ECPublicKey to another as points and returns the resulting ECPublicKey.
    *
    * Note: if this ever becomes a bottleneck, secp256k1_ec_pubkey_combine should
    * get wrapped in NativeSecp256k1 to speed things up.
    */
  def add(otherKey: ECPublicKey): ECPublicKey = {
    addWithBouncyCastle(otherKey)
  }

  def addWithBouncyCastle(otherKey: ECPublicKey): ECPublicKey = {
    val sumPoint = toPoint.add(otherKey.toPoint)

    ECPublicKey.fromPoint(sumPoint)
  }

  def tweakMultiply(tweak: FieldElement): ECPublicKey = {
    tweakMultiply(tweak, CryptoContext.default)
  }

  def tweakMultiply(
      tweak: FieldElement,
      context: CryptoContext): ECPublicKey = {
    context match {
      case CryptoContext.LibSecp256k1 => tweakMultiplyWithSecp(tweak)
      case CryptoContext.BouncyCastle => tweakMultiplyWithBouncyCastle(tweak)
    }
  }

  def tweakMultiplyWithSecp(tweak: FieldElement): ECPublicKey = {
    val mulBytes = NativeSecp256k1.pubKeyTweakMul(bytes.toArray,
                                                  tweak.bytes.toArray,
                                                  isCompressed)
    ECPublicKey(ByteVector(mulBytes))
  }

  def tweakMultiplyWithBouncyCastle(tweak: FieldElement): ECPublicKey = {
    BouncyCastleUtil.pubKeyTweakMul(this, tweak.bytes)
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

  /** Generates a fresh [[org.bitcoins.crypto.ECPublicKey ECPublicKey]] that has not been used before. */
  def freshPublicKey: ECPublicKey = ECPrivateKey.freshPrivateKey.publicKey

  /**
    * Checks if the public key is valid according to secp256k1
    * Mimics this function in bitcoin core
    * [[https://github.com/bitcoin/bitcoin/blob/27765b6403cece54320374b37afb01a0cfe571c3/src/pubkey.cpp#L207-L212]]
    */
  def isFullyValid(bytes: ByteVector): Boolean = {
    isFullyValid(bytes, CryptoContext.default)
  }

  def isFullyValid(bytes: ByteVector, context: CryptoContext): Boolean = {
    context match {
      case CryptoContext.LibSecp256k1 => isFullyValidWithSecp(bytes)
      case CryptoContext.BouncyCastle => isFullyValidWithBouncyCastle(bytes)
    }
  }

  def isFullyValidWithSecp(bytes: ByteVector): Boolean = {
    try {
      NativeSecp256k1.isValidPubKey(bytes.toArray) && isValid(bytes)
    } catch {
      case scala.util.control.NonFatal(_) =>
        false
    }
  }

  def isFullyValidWithBouncyCastle(bytes: ByteVector): Boolean = {
    BouncyCastleUtil.validatePublicKey(bytes) && isValid(bytes)
  }

  /**
    * Mimics the CPubKey::IsValid function in Bitcoin core, this is a consensus rule
    * [[https://github.com/bitcoin/bitcoin/blob/27765b6403cece54320374b37afb01a0cfe571c3/src/pubkey.h#L158]]
    */
  def isValid(bytes: ByteVector): Boolean = bytes.nonEmpty

  /** Creates a [[org.bitcoins.crypto.ECPublicKey ECPublicKey]] from the
    * [[org.bouncycastle.math.ec.ECPoint ECPoint]] data structure used internally inside of Bouncy Castle
    */
  def fromPoint(p: ECPoint, isCompressed: Boolean = true): ECPublicKey = {
    val bytes = p.getEncoded(isCompressed)
    ECPublicKey.fromBytes(ByteVector(bytes))
  }
}
