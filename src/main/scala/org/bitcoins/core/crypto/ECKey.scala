package org.bitcoins.core.crypto

import java.math.BigInteger
import java.security.SecureRandom

import org.bitcoin.NativeSecp256k1
import org.bitcoins.core.config.{ NetworkParameters, Networks }
import org.bitcoins.core.protocol.NetworkElement
import org.bitcoins.core.util.{ BitcoinSUtil, _ }
import org.bouncycastle.crypto.AsymmetricCipherKeyPair
import org.bouncycastle.crypto.digests.SHA256Digest
import org.bouncycastle.crypto.generators.ECKeyPairGenerator
import org.bouncycastle.crypto.params.{ ECKeyGenerationParameters, ECPrivateKeyParameters, ECPublicKeyParameters }
import org.bouncycastle.crypto.signers.{ ECDSASigner, HMacDSAKCalculator }

import scala.annotation.tailrec
import scala.util.{ Failure, Success, Try }

/**
 * Created by chris on 2/16/16.
 */
sealed abstract class BaseECKey extends NetworkElement {
  /**
   * Signs a given sequence of bytes with the signingKey
   * @param dataToSign the bytes to be signed
   * @param signingKey the key to sign the bytes with
   * @return the digital signature
   */
  private def sign(dataToSign: Seq[Byte], signingKey: BaseECKey): ECDigitalSignature = {
    require(dataToSign.length == 32 && signingKey.bytes.length <= 32)
    val signature = NativeSecp256k1.sign(dataToSign.toArray, signingKey.bytes.toArray)
    ECDigitalSignature(signature)
  }

  def sign(dataToSign: Seq[Byte]): ECDigitalSignature = sign(dataToSign, this)

  def sign(hash: HashDigest, signingKey: BaseECKey): ECDigitalSignature = sign(hash.bytes, signingKey)

  def sign(hash: HashDigest): ECDigitalSignature = sign(hash, this)

  @deprecated("Deprecated in favor of signing algorithm inside of secp256k1", "2/20/2017")
  private def oldSign(dataToSign: Seq[Byte], signingKey: BaseECKey): ECDigitalSignature = {
    val signer: ECDSASigner = new ECDSASigner(new HMacDSAKCalculator(new SHA256Digest()))
    val privKey: ECPrivateKeyParameters = new ECPrivateKeyParameters(
      new BigInteger(1, signingKey.bytes.toArray), CryptoParams.curve
    )
    signer.init(true, privKey)
    val components: Array[BigInteger] = signer.generateSignature(dataToSign.toArray)
    val (r, s) = (components(0), components(1))
    val signature = ECDigitalSignature(r, s)
    //make sure the signature follows BIP62's low-s value
    //https://github.com/bitcoin/bips/blob/master/bip-0062.mediawiki#Low_S_values_in_signatures
    //bitcoinj implementation
    //https://github.com/bitcoinj/bitcoinj/blob/1e66b9a8e38d9ad425507bf5f34d64c5d3d23bb8/core/src/main/java/org/bitcoinj/core/ECKey.java#L551
    val signatureLowS = DERSignatureUtil.lowS(signature)
    require(signatureLowS.isDEREncoded, "We must create DER encoded signatures when signing a piece of data, got: " + signatureLowS)
    signatureLowS
  }
}

/**
 * Created by chris on 2/16/16.
 */
sealed abstract class ECPrivateKey extends BaseECKey {
  /** Signifies if the this private key corresponds to a compressed public key */
  def isCompressed: Boolean

  /** Derives the public for a the private key */
  def publicKey: ECPublicKey = {
    val pubKeyBytes: Seq[Byte] = NativeSecp256k1.computePubkey(bytes.toArray, isCompressed)
    require(NativeSecp256k1.isValidPubKey(pubKeyBytes.toArray), "secp256k1 failed to generate a valid public key, got: " + BitcoinSUtil.encodeHex(pubKeyBytes))
    ECPublicKey(pubKeyBytes)
  }

  /**
   * Converts a [[ECPrivateKey]] to WIF
   * https://en.bitcoin.it/wiki/Wallet_import_format
   * @return
   */
  def toWIF(network: NetworkParameters): String = {
    val networkByte = network.privateKey
    //append 1 byte to the end of the priv key byte representation if we need a compressed pub key
    val fullBytes = if (isCompressed) networkByte ++ (bytes ++ Seq(1.toByte)) else networkByte ++ bytes
    val hash = CryptoUtil.doubleSHA256(fullBytes)
    val checksum = hash.bytes.take(4)
    val encodedPrivKey = fullBytes ++ checksum
    Base58.encode(encodedPrivKey)
  }

  override def toString = "ECPrivateKey(" + hex + "," + isCompressed + ")"
}

object ECPrivateKey extends Factory[ECPrivateKey] {

  private case class ECPrivateKeyImpl(override val bytes: Seq[Byte], isCompressed: Boolean) extends ECPrivateKey {
    require(NativeSecp256k1.secKeyVerify(bytes.toArray), "Invalid key according to secp256k1, hex: " + BitcoinSUtil.encodeHex(bytes))
  }

  override def fromBytes(bytes: Seq[Byte]): ECPrivateKey = fromBytes(bytes, true)

  @tailrec
  def fromBytes(bytes: Seq[Byte], isCompressed: Boolean): ECPrivateKey = {
    if (bytes.size == 32) ECPrivateKeyImpl(bytes, isCompressed)
    else if (bytes.size < 32) {
      //means we need to pad the private key with 0 bytes so we have 32 bytes
      val paddingNeeded = 32 - bytes.size
      val padding = for { _ <- 0 until paddingNeeded } yield 0.toByte
      ECPrivateKey.fromBytes(padding ++ bytes, isCompressed)
    } //this is for the case when java serialies a BigInteger to 33 bytes to hold the signed num representation
    else if (bytes.size == 33) ECPrivateKey.fromBytes(bytes.slice(1, 33), isCompressed)
    else throw new IllegalArgumentException("Private keys cannot be greater than 33 bytes in size, got: " +
      BitcoinSUtil.encodeHex(bytes) + " which is of size: " + bytes.size)
  }

  def fromHex(hex: String, isCompressed: Boolean): ECPrivateKey = fromBytes(BitcoinSUtil.decodeHex(hex), isCompressed)

  /** Generates a fresh [[ECPrivateKey]] that has not been used before. */
  def apply(): ECPrivateKey = ECPrivateKey(true)

  def apply(isCompressed: Boolean) = freshPrivateKey(isCompressed)

  /** Generates a fresh [[ECPrivateKey]] that has not been used before. */
  def freshPrivateKey: ECPrivateKey = freshPrivateKey(true)

  def freshPrivateKey(isCompressed: Boolean): ECPrivateKey = {
    val secureRandom = new SecureRandom
    val generator: ECKeyPairGenerator = new ECKeyPairGenerator
    val keyGenParams: ECKeyGenerationParameters = new ECKeyGenerationParameters(CryptoParams.curve, secureRandom)
    generator.init(keyGenParams)
    val keypair: AsymmetricCipherKeyPair = generator.generateKeyPair
    val privParams: ECPrivateKeyParameters = keypair.getPrivate.asInstanceOf[ECPrivateKeyParameters]
    val priv: BigInteger = privParams.getD
    val bytes = priv.toByteArray
    ECPrivateKey.fromBytes(bytes, isCompressed)
  }
  /**
   * Takes in a base58 string and converts it into a private key.
   * Private keys starting with 'K', 'L', or 'c' correspond to compressed public keys.
   * https://en.bitcoin.it/wiki/Wallet_import_format
   *
   * @param WIF Wallet Import Format. Encoded in Base58
   * @return
   */
  def fromWIFToPrivateKey(WIF: String): ECPrivateKey = {
    val isCompressed = ECPrivateKey.isCompressed(WIF)
    val privateKeyBytes = trimFunction(WIF)
    ECPrivateKey.fromBytes(privateKeyBytes, isCompressed)
  }

  /**
   * Takes in WIF private key as a sequence of bytes and determines if it corresponds to a compressed public key.
   * If the private key corresponds to a compressed public key, the last byte should be 0x01, and
   * the WIF string will have started with K or L instead of 5 (or c instead of 9 on testnet).
   *
   * @param bytes private key in bytes
   * @return
   */
  def isCompressed(bytes: Seq[Byte]): Boolean = {
    val validCompressedBytes: Seq[Seq[Byte]] = Networks.secretKeyBytes
    val validCompressedBytesInHex: Seq[String] = validCompressedBytes.map(b => BitcoinSUtil.encodeHex(b))
    val firstByteHex = BitcoinSUtil.encodeHex(bytes.head)
    if (validCompressedBytesInHex.contains(firstByteHex)) bytes(bytes.length - 5) == 0x01.toByte
    else false
  }

  def isCompressed(WIF: String): Boolean = {
    val bytes = Base58.decode(WIF)
    isCompressed(bytes)
  }

  /**
   * When decoding a WIF private key, we drop the first byte (network byte), and the last 4 bytes (checksum).
   * If the private key corresponds to a compressed public key, we drop the last byte again.
   * https://en.bitcoin.it/wiki/Wallet_import_format
   * @param WIF Wallet Import Format. Encoded in Base58
   * @return
   */
  private def trimFunction(WIF: String): Seq[Byte] = {
    val bytesChecked = Base58.decodeCheck(WIF)

    //see https://en.bitcoin.it/wiki/List_of_address_prefixes
    //for where '5' and '9' come from
    bytesChecked match {
      case Success(bytes) if uncompressedKeyPrefixes.contains(WIF.headOption) => bytes.tail
      case Success(bytes) if isCompressed(WIF) => bytes.tail.dropRight(1)
      case Success(bytes) => bytes.tail
      case Failure(exception) => throw exception
    }
  }

  /** The Base58 prefixes that represent compressed private keys */
  def compressedKeyPrefixes = Seq(Some('K'), Some('L'), Some('c'))

  /** The Base58 prefixes that represent uncompressed private keys */
  def uncompressedKeyPrefixes = Seq(Some('5'), Some('9'))

  /** Returns the [[NetworkParameters]] from a serialized WIF key */
  def parseNetworkFromWIF(wif: String): Try[NetworkParameters] = {
    val decoded = Base58.decodeCheck(wif)
    decoded match {
      case Success(bytes) =>
        val networkMatch = Networks.secretKeyBytes.find(b => bytes.startsWith(b))
        if (networkMatch.isDefined) {
          Success(Networks.bytesToNetwork(networkMatch.get))
        } else {
          Failure(new IllegalArgumentException("Failed to match network bytes for WIF"))
        }
      case Failure(exn) => Failure(exn)
    }
  }

}

/**
 * Created by chris on 2/16/16.
 */
sealed abstract class ECPublicKey extends BaseECKey {
  def verify(hash: HashDigest, signature: ECDigitalSignature): Boolean = verify(hash.bytes, signature)

  /** Verifies if a given piece of data is signed by the [[ECPrivateKey]]'s corresponding [[ECPublicKey]]. */
  def verify(data: Seq[Byte], signature: ECDigitalSignature): Boolean = {
    logger.debug("PubKey for verifying: " + BitcoinSUtil.encodeHex(bytes))
    logger.debug("Data to verify: " + BitcoinSUtil.encodeHex(data))
    logger.debug("Signature to check against data: " + signature.hex)
    val result = NativeSecp256k1.verify(data.toArray, signature.bytes.toArray, bytes.toArray)
    if (!result) {
      //if signature verification fails with libsecp256k1 we need to use our old
      //verification function from spongy castle, this is needed because early blockchain
      //transactions can have weird non strict der encoded digital signatures
      //bitcoin core implements this functionality here:
      //https://github.com/bitcoin/bitcoin/blob/master/src/pubkey.cpp#L16-L165
      //TODO: Implement functionality in Bitcoin Core linked above
      oldVerify(data, signature)
    } else result
  }

  def verify(hex: String, signature: ECDigitalSignature): Boolean = verify(BitcoinSUtil.decodeHex(hex), signature)

  override def toString = "ECPublicKey(" + hex + ")"

  @deprecated("Deprecated in favor of using verify functionality inside of secp256k1", "2/20/2017")
  private def oldVerify(data: Seq[Byte], signature: ECDigitalSignature): Boolean = {
    /** The elliptic curve used by bitcoin. */
    def curve = CryptoParams.curve
    /** This represents this public key in the bouncy castle library */
    def publicKeyParams = new ECPublicKeyParameters(curve.getCurve.decodePoint(bytes.toArray), curve)

    val resultTry = Try {
      val signer = new ECDSASigner
      signer.init(false, publicKeyParams)
      signature match {
        case EmptyDigitalSignature => signer.verifySignature(data.toArray, java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(0))
        case sig: ECDigitalSignature =>
          logger.debug("Public key bytes: " + BitcoinSUtil.encodeHex(bytes))
          val rBigInteger: BigInteger = new BigInteger(signature.r.toString())
          val sBigInteger: BigInteger = new BigInteger(signature.s.toString())
          signer.verifySignature(data.toArray, rBigInteger, sBigInteger)
      }
    }
    resultTry.getOrElse(false)
  }

  /** Checks if the [[ECPublicKey]] is compressed */
  def isCompressed: Boolean = bytes.size == 33

  /** Checks if the [[ECPublicKey]] is valid according to secp256k1 */
  def isFullyValid = ECPublicKey.isFullyValid(bytes)
}

object ECPublicKey extends Factory[ECPublicKey] {

  private case class ECPublicKeyImpl(override val bytes: Seq[Byte]) extends ECPublicKey {
    //unfortunately we cannot place ANY invariants here
    //because of old transactions on the blockchain that have weirdly formatted public keys. Look at example in script_tests.json
    //https://github.com/bitcoin/bitcoin/blob/master/src/test/data/script_tests.json#L457
    //bitcoin core only checks CPubKey::IsValid()
    //this means we can have public keys with only one byte i.e. 0x00 or no bytes.
    //Eventually we would like this to be CPubKey::IsFullyValid() but since we are remaining backwards compatible
    //we cannot do this. If there ever is a hard fork this would be a good thing to add.
  }

  override def fromBytes(bytes: Seq[Byte]): ECPublicKey = ECPublicKeyImpl(bytes)

  def apply() = freshPublicKey

  /** Generates a fresh [[ECPublicKey]] that has not been used before. */
  def freshPublicKey = ECPrivateKey.freshPrivateKey.publicKey

  /**
   * Checks if the public key is valid according to secp256k1
   * Mimics this function in bitcoin core
   * [[https://github.com/bitcoin/bitcoin/blob/27765b6403cece54320374b37afb01a0cfe571c3/src/pubkey.cpp#L207-L212]]
   */
  def isFullyValid(bytes: Seq[Byte]): Boolean = Try(NativeSecp256k1.isValidPubKey(bytes.toArray)).isSuccess && isValid(bytes)

  /**
   * Mimics the CPubKey::IsValid function in Bitcoin core, this is a consensus rule
   * [[https://github.com/bitcoin/bitcoin/blob/27765b6403cece54320374b37afb01a0cfe571c3/src/pubkey.h#L158]]
   */
  def isValid(bytes: Seq[Byte]): Boolean = bytes.nonEmpty
}
