package org.bitcoins.crypto

import org.bitcoin.NativeSecp256k1
import scodec.bits.ByteVector

/** This is an implementation of [[CryptoRuntime]] that defaults to libsecp256k1
  * (https://github.com/bitcoin-core/secp256k1) when possible. All unsupported functions
  * are delegated to [[BouncycastleCryptoRuntime]].
  */
trait LibSecp256k1CryptoRuntime extends CryptoRuntime {

  override val cryptoContext: CryptoContext = CryptoContext.LibSecp256k1

  override def freshPrivateKey: ECPrivateKey =
    BouncycastleCryptoRuntime.freshPrivateKey

  override def recoverPublicKey(
      signature: ECDigitalSignature,
      message: ByteVector): (ECPublicKey, ECPublicKey) =
    BouncycastleCryptoRuntime.recoverPublicKey(signature, message)

  override def hmac512(key: ByteVector, data: ByteVector): ByteVector =
    BouncycastleCryptoRuntime.hmac512(key, data)

  override def ripeMd160(bytes: ByteVector): RipeMd160Digest = {
    BouncycastleCryptoRuntime.ripeMd160(bytes)
  }

  override def sha256(bytes: ByteVector): Sha256Digest = {
    BouncycastleCryptoRuntime.sha256(bytes)
  }

  override def sha1(bytes: ByteVector): Sha1Digest = {
    BouncycastleCryptoRuntime.sha1(bytes)
  }

  override def normalize(str: String): String = {
    BouncycastleCryptoRuntime.normalize(str)
  }

  override def sha256Hash160(bytes: ByteVector): Sha256Hash160Digest = {
    BouncycastleCryptoRuntime.sha256Hash160(bytes)
  }

  override def toPublicKey(
      privateKey: ECPrivateKey,
      isCompressed: Boolean): ECPublicKey = {
    val pubKeyBytes: Array[Byte] =
      NativeSecp256k1.computePubkey(privateKey.bytes.toArray, isCompressed)
    val pubBytes = ByteVector(pubKeyBytes)
    require(
      ECPublicKey.isFullyValid(pubBytes),
      s"secp256k1 failed to generate a valid public key, got: ${CryptoBytesUtil
        .encodeHex(pubBytes)}")
    ECPublicKey(pubBytes)
  }

  override def sign(
      privateKey: ECPrivateKey,
      dataToSign: ByteVector): ECDigitalSignature = {
    val signature =
      NativeSecp256k1.sign(dataToSign.toArray, privateKey.bytes.toArray)
    ECDigitalSignature(ByteVector(signature))
  }

  override def signWithEntropy(
      privateKey: ECPrivateKey,
      bytes: ByteVector,
      entropy: ByteVector): ECDigitalSignature = {
    val sigBytes = NativeSecp256k1.signWithEntropy(bytes.toArray,
                                                   privateKey.bytes.toArray,
                                                   entropy.toArray)

    ECDigitalSignature(ByteVector(sigBytes))
  }

  override def secKeyVerify(privateKeyBytes: ByteVector): Boolean =
    NativeSecp256k1.secKeyVerify(privateKeyBytes.toArray)

  override def verify(
      publicKey: ECPublicKey,
      data: ByteVector,
      signature: ECDigitalSignature): Boolean = {
    val result =
      NativeSecp256k1.verify(data.toArray,
                             signature.bytes.toArray,
                             publicKey.bytes.toArray)

    if (!result) {
      //if signature verification fails with libsecp256k1 we need to use our old
      //verification function from spongy castle, this is needed because early blockchain
      //transactions can have weird non strict der encoded digital signatures
      //bitcoin core implements this functionality here:
      //https://github.com/bitcoin/bitcoin/blob/master/src/pubkey.cpp#L16-L165

      BouncycastleCryptoRuntime.verify(publicKey, data, signature)
    } else result
  }

  override def decompressed(publicKey: ECPublicKey): ECPublicKey = {
    if (publicKey.isCompressed) {
      val decompressed = NativeSecp256k1.decompress(publicKey.bytes.toArray)
      ECPublicKey.fromBytes(ByteVector(decompressed))
    } else publicKey
  }

  override def publicKey(privateKey: ECPrivateKey): ECPublicKey = {
    val pubKeyBytes: Array[Byte] =
      NativeSecp256k1.computePubkey(privateKey.bytes.toArray,
                                    privateKey.isCompressed)
    val pubBytes = ByteVector(pubKeyBytes)
    require(
      ECPublicKey.isFullyValid(pubBytes),
      s"secp256k1 failed to generate a valid public key, got: ${CryptoBytesUtil
        .encodeHex(pubBytes)}")
    ECPublicKey(pubBytes)
  }

  override def tweakMultiply(
      publicKey: ECPublicKey,
      tweak: FieldElement): ECPublicKey = {
    val mulBytes = NativeSecp256k1.pubKeyTweakMul(publicKey.bytes.toArray,
                                                  tweak.bytes.toArray,
                                                  publicKey.isCompressed)
    ECPublicKey(ByteVector(mulBytes))
  }

  override def add(pk1: ECPrivateKey, pk2: ECPrivateKey): ECPrivateKey = {
    val sumBytes =
      NativeSecp256k1.privKeyTweakAdd(pk1.bytes.toArray, pk2.bytes.toArray)
    ECPrivateKey(ByteVector(sumBytes))
  }

  override def add(pk1: ByteVector, pk2: ECPrivateKey): ByteVector = {
    val sum =
      NativeSecp256k1.privKeyTweakAdd(pk1.toArray, pk2.bytes.toArray)
    ByteVector(sum)
  }

  override def add(pk1: ECPublicKey, pk2: ECPublicKey): ECPublicKey =
    BouncycastleCryptoRuntime.add(pk1, pk2)

  override def pubKeyTweakAdd(
      pubkey: ECPublicKey,
      privkey: ECPrivateKey): ECPublicKey = {
    val tweaked = NativeSecp256k1.pubKeyTweakAdd(pubkey.bytes.toArray,
                                                 privkey.bytes.toArray,
                                                 privkey.isCompressed)
    ECPublicKey(ByteVector(tweaked))
  }

  override def isValidPubKey(bytes: ByteVector): Boolean = {
    try {
      NativeSecp256k1.isValidPubKey(bytes.toArray)
    } catch {
      case scala.util.control.NonFatal(_) =>
        false
    }
  }

  override def isFullyValidWithBouncyCastle(bytes: ByteVector): Boolean =
    bytes.nonEmpty && BouncycastleCryptoRuntime.isValidPubKey(bytes)

  // TODO: add a native implementation
  override def schnorrSign(
      dataToSign: ByteVector,
      privateKey: ECPrivateKey,
      auxRand: ByteVector): SchnorrDigitalSignature = {
//    val sigBytes =
//      NativeSecp256k1.schnorrSign(dataToSign.toArray,
//                                  privateKey.bytes.toArray,
//                                  auxRand.toArray)
//    SchnorrDigitalSignature(ByteVector(sigBytes))
    BouncycastleCryptoRuntime.schnorrSign(dataToSign, privateKey, auxRand)
  }

  // TODO: add a native implementation
  override def schnorrSignWithNonce(
      dataToSign: ByteVector,
      privateKey: ECPrivateKey,
      nonceKey: ECPrivateKey): SchnorrDigitalSignature = {
//    val sigBytes =
//      NativeSecp256k1.schnorrSignWithNonce(dataToSign.toArray,
//                                           privateKey.bytes.toArray,
//                                           nonceKey.bytes.toArray)
//    SchnorrDigitalSignature(ByteVector(sigBytes))
    BouncycastleCryptoRuntime.schnorrSignWithNonce(dataToSign,
                                                   privateKey,
                                                   nonceKey)
  }

  // TODO: add a native implementation
  override def schnorrVerify(
      data: ByteVector,
      schnorrPubKey: SchnorrPublicKey,
      signature: SchnorrDigitalSignature): Boolean = {
    BouncycastleCryptoRuntime.schnorrVerify(data, schnorrPubKey, signature)
  }

  // TODO: add a native implementation
  override def schnorrComputeSigPoint(
      data: ByteVector,
      nonce: SchnorrNonce,
      pubKey: SchnorrPublicKey,
      compressed: Boolean): ECPublicKey = {
    BouncycastleCryptoRuntime.schnorrComputeSigPoint(data,
                                                     nonce,
                                                     pubKey,
                                                     compressed)
  }

  // TODO: add a native implementation
  override def adaptorSign(
      key: ECPrivateKey,
      adaptorPoint: ECPublicKey,
      msg: ByteVector): ECAdaptorSignature = {
//    val sigWithProof = NativeSecp256k1.adaptorSign(key.bytes.toArray,
//                                                   adaptorPoint.bytes.toArray,
//                                                   msg.toArray)
//    ECAdaptorSignature(ByteVector(sigWithProof))
    BouncycastleCryptoRuntime.adaptorSign(key, adaptorPoint, msg)
  }

  // TODO: add a native implementation
  override def adaptorComplete(
      key: ECPrivateKey,
      adaptorSignature: ECAdaptorSignature): ECDigitalSignature = {
//    val sigBytes =
//      NativeSecp256k1.adaptorAdapt(key.bytes.toArray,
//                                   adaptorSignature.adaptedSig.toArray)
//    ECDigitalSignature.fromBytes(ByteVector(sigBytes))
    BouncycastleCryptoRuntime.adaptorComplete(key, adaptorSignature)
  }

  // TODO: add a native implementation
  override def extractAdaptorSecret(
      signature: ECDigitalSignature,
      adaptorSignature: ECAdaptorSignature,
      key: ECPublicKey): ECPrivateKey = {
//    val secretBytes = NativeSecp256k1.adaptorExtractSecret(
//      signature.bytes.toArray,
//      adaptorSignature.adaptedSig.toArray,
//      key.bytes.toArray)
//
//    ECPrivateKey(ByteVector(secretBytes))
    BouncycastleCryptoRuntime.extractAdaptorSecret(signature,
                                                   adaptorSignature,
                                                   key)
  }

  // TODO: add a native implementation
  override def adaptorVerify(
      adaptorSignature: ECAdaptorSignature,
      key: ECPublicKey,
      msg: ByteVector,
      adaptorPoint: ECPublicKey): Boolean = {
//    NativeSecp256k1.adaptorVerify(adaptorSignature.adaptedSig.toArray,
//                                  key.bytes.toArray,
//                                  msg.toArray,
//                                  adaptorPoint.bytes.toArray,
//                                  adaptorSignature.dleqProof.toArray)
    BouncycastleCryptoRuntime.adaptorVerify(adaptorSignature,
                                            key,
                                            msg,
                                            adaptorPoint)
  }

  override def decodeSignature(
      signature: ECDigitalSignature): (BigInt, BigInt) =
    BouncycastleCryptoRuntime.decodeSignature(signature)

  override def isValidSignatureEncoding(
      signature: ECDigitalSignature): Boolean =
    BouncycastleCryptoRuntime.isValidSignatureEncoding(signature)

  override def isDEREncoded(signature: ECDigitalSignature): Boolean =
    BouncycastleCryptoRuntime.isDEREncoded(signature)

  override def sipHash(item: ByteVector, key: SipHashKey): Long =
    BouncycastleCryptoRuntime.sipHash(item, key)
}

object LibSecp256k1CryptoRuntime extends LibSecp256k1CryptoRuntime
