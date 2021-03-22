package org.bitcoins.crypto

import org.bouncycastle.crypto.digests.SHA256Digest
import org.bouncycastle.crypto.params.{
  ECPrivateKeyParameters,
  ECPublicKeyParameters
}
import org.bouncycastle.crypto.signers.{ECDSASigner, HMacDSAKCalculator}
import org.bouncycastle.math.ec.ECCurve
import scodec.bits.ByteVector

import java.math.BigInteger
import scala.util.Try

object BouncyCastleUtil {

  private val curve: ECCurve = BouncyCastleCryptoParams.curve.getCurve
  private val G = BouncyCastleCryptoParams.curve.getG

  private def getBigInteger(bytes: ByteVector): BigInteger = {
    new BigInteger(1, bytes.toArray)
  }

  def pubKeyTweakMul(publicKey: ECPublicKey, tweak: ByteVector): ECPublicKey = {
    val point = decodePoint(publicKey).multiply(getBigInteger(tweak))
    decodePubKey(point, publicKey.isCompressed)
  }

  private[crypto] def decodePoint(
      bytes: ByteVector): org.bouncycastle.math.ec.ECPoint = {
    curve.decodePoint(bytes.toArray)
  }

  private[crypto] def decodePoint(
      pubKey: ECPublicKey): org.bouncycastle.math.ec.ECPoint = {
    decodePoint(pubKey.bytes)
  }

  private[crypto] def decodePubKey(
      point: org.bouncycastle.math.ec.ECPoint,
      isCompressed: Boolean = true): ECPublicKey = {
    val bytes = point.getEncoded(isCompressed)
    ECPublicKey.fromBytes(ByteVector(bytes))
  }

  def validatePublicKey(bytes: ByteVector): Boolean = {
    Try(decodePoint(bytes))
      .map(_.getCurve == curve)
      .getOrElse(false)
  }

  def pubKeyTweakMul(pubKey: ECPublicKey, tweak: FieldElement): ECPublicKey = {
    val tweakedPoint = decodePoint(pubKey).multiply(tweak.toBigInteger)
    decodePubKey(tweakedPoint, pubKey.isCompressed)
  }

  def decompressPublicKey(publicKey: ECPublicKey): ECPublicKey = {
    if (publicKey.isCompressed) {
      val point = decodePoint(publicKey.bytes)
      val decompressedBytes =
        ByteVector.fromHex("04").get ++
          ByteVector(point.getXCoord.getEncoded) ++
          ByteVector(point.getYCoord.getEncoded)
      ECPublicKey(decompressedBytes)
    } else publicKey
  }

  def computePublicKey(privateKey: ECPrivateKey): ECPublicKey = {
    val priv = getBigInteger(privateKey.bytes)
    val point = G.multiply(priv)
    val pubBytes = ByteVector(point.getEncoded(privateKey.isCompressed))
    require(
      ECPublicKey.isFullyValid(pubBytes),
      s"Bouncy Castle failed to generate a valid public key, got: ${CryptoBytesUtil
        .encodeHex(pubBytes)}")
    ECPublicKey(pubBytes)
  }

  def publicKeyConvert(key: ECPublicKey, compressed: Boolean): ECPublicKey = {
    if (key.isCompressed == compressed) {
      key
    } else {
      val point = decodePoint(key)
      val pubBytes = ByteVector(point.getEncoded(compressed))
      require(
        ECPublicKey.isFullyValid(pubBytes),
        s"Bouncy Castle failed to generate a valid public key, got: ${CryptoBytesUtil
          .encodeHex(pubBytes)}")
      ECPublicKey(pubBytes)
    }
  }

  def sign(
      dataToSign: ByteVector,
      privateKey: ECPrivateKey): ECDigitalSignature = {
    val signer: ECDSASigner = new ECDSASigner(
      new HMacDSAKCalculator(new SHA256Digest()))
    val privKey: ECPrivateKeyParameters =
      new ECPrivateKeyParameters(getBigInteger(privateKey.bytes),
                                 BouncyCastleCryptoParams.curve)
    signer.init(true, privKey)
    val components: Array[BigInteger] =
      signer.generateSignature(dataToSign.toArray)
    val (r, s) = (components(0), components(1))
    val signature = ECDigitalSignature(r, s)
    //make sure the signature follows BIP62's low-s value
    //https://github.com/bitcoin/bips/blob/master/bip-0062.mediawiki#Low_S_values_in_signatures
    //bitcoinj implementation
    //https://github.com/bitcoinj/bitcoinj/blob/1e66b9a8e38d9ad425507bf5f34d64c5d3d23bb8/core/src/main/java/org/bitcoinj/core/ECKey.java#L551
    val signatureLowS = DERSignatureUtil.lowS(signature)
    require(
      signatureLowS.isDEREncoded,
      "We must create DER encoded signatures when signing a piece of data, got: " + signatureLowS)
    signatureLowS
  }

  /** Create an ECDSA signature adding specified entropy.
    *
    * This can be used to include your own entropy to nonce generation
    * in addition to the message and private key, while still doing so deterministically.
    *
    * In particular, this is used when generating low R signatures.
    * @see [[https://github.com/bitcoin/bitcoin/pull/13666/]]
    */
  def signWithEntropy(
      dataToSign: ByteVector,
      privateKey: ECPrivateKey,
      entropy: ByteVector): ECDigitalSignature = {
    val signer: ECDSASigner = new ECDSASigner(
      new HMacDSAKCalculatorWithEntropy(new SHA256Digest(), entropy))
    val privKey: ECPrivateKeyParameters =
      new ECPrivateKeyParameters(getBigInteger(privateKey.bytes),
                                 BouncyCastleCryptoParams.curve)
    signer.init(true, privKey)
    val components: Array[BigInteger] =
      signer.generateSignature(dataToSign.toArray)
    val (r, s) = (components(0), components(1))
    val signature = ECDigitalSignature(r, s)
    //make sure the signature follows BIP62's low-s value
    //https://github.com/bitcoin/bips/blob/master/bip-0062.mediawiki#Low_S_values_in_signatures
    //bitcoinj implementation
    //https://github.com/bitcoinj/bitcoinj/blob/1e66b9a8e38d9ad425507bf5f34d64c5d3d23bb8/core/src/main/java/org/bitcoinj/core/ECKey.java#L551
    val signatureLowS = DERSignatureUtil.lowS(signature)
    require(
      signatureLowS.isDEREncoded,
      "We must create DER encoded signatures when signing a piece of data, got: " + signatureLowS)
    signatureLowS
  }

  def verifyDigitalSignature(
      data: ByteVector,
      publicKey: ECPublicKey,
      signature: ECDigitalSignature): Boolean = {
    val resultTry = Try {
      val publicKeyParams =
        new ECPublicKeyParameters(decodePoint(publicKey.bytes),
                                  BouncyCastleCryptoParams.curve)

      val signer = new ECDSASigner
      signer.init(false, publicKeyParams)
      signature match {
        case EmptyDigitalSignature =>
          signer.verifySignature(data.toArray,
                                 java.math.BigInteger.valueOf(0),
                                 java.math.BigInteger.valueOf(0))
        case _: ECDigitalSignature =>
          val (r, s) = signature.decodeSignature
          signer.verifySignature(data.toArray, r.bigInteger, s.bigInteger)
      }
    }
    resultTry.getOrElse(false)
  }
}
