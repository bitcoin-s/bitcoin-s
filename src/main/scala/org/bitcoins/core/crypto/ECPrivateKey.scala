package org.bitcoins.core.crypto

import java.math.BigInteger
import java.security.SecureRandom

import org.bitcoin.NativeSecp256k1
import org.bitcoins.core.config.{MainNet, NetworkParameters, TestNet3}
import org.bitcoins.core.protocol.blockchain.{MainNetChainParams, SecretKey, TestNetChainParams}
import org.bitcoins.core.util._
import org.spongycastle.crypto.AsymmetricCipherKeyPair
import org.spongycastle.crypto.generators.ECKeyPairGenerator
import org.spongycastle.crypto.params.{ECKeyGenerationParameters, ECPrivateKeyParameters}

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

/**
 * Created by chris on 2/16/16.
 */
sealed trait ECPrivateKey extends BaseECKey {
  /** Signifies if the this private key corresponds to a compressed public key */
  def isCompressed : Boolean

  /** Derives the public for a the private key */
  def publicKey : ECPublicKey = {
    val pubKeyBytes : Seq[Byte] = NativeSecp256k1.computePubkey(bytes.toArray, isCompressed)
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
    val fullBytes = if (isCompressed) networkByte +: (bytes ++ Seq(1.toByte)) else networkByte +: bytes
    val hash = CryptoUtil.doubleSHA256(fullBytes)
    val checksum = hash.bytes.take(4)
    val encodedPrivKey = fullBytes ++ checksum
    Base58.encode(encodedPrivKey)
  }

  override def toString = "ECPrivateKey(" + hex + "," + isCompressed +")"
}

object ECPrivateKey extends Factory[ECPrivateKey] {

  private case class ECPrivateKeyImpl(bytes : Seq[Byte], isCompressed: Boolean) extends ECPrivateKey {
    require(NativeSecp256k1.secKeyVerify(bytes.toArray), "Invalid key according to secp256k1, hex: " + BitcoinSUtil.encodeHex(bytes))
  }

  override def fromBytes(bytes : Seq[Byte]) : ECPrivateKey = fromBytes(bytes,true)

  @tailrec
  def fromBytes(bytes: Seq[Byte], isCompressed: Boolean): ECPrivateKey = {
    if (bytes.size == 32) ECPrivateKeyImpl(bytes,isCompressed)
    else if (bytes.size < 32) {
      //means we need to pad the private key with 0 bytes so we have 32 bytes
      val paddingNeeded = 32 - bytes.size
      val padding = for { _ <- 0 until paddingNeeded} yield 0.toByte
      ECPrivateKey.fromBytes(padding ++ bytes, isCompressed)
    }
    //this is for the case when java serialies a BigInteger to 33 bytes to hold the signed num representation
    else if (bytes.size == 33) ECPrivateKey.fromBytes(bytes.slice(1,33), isCompressed)
    else throw new IllegalArgumentException("Private keys cannot be greater than 33 bytes in size, got: " +
      BitcoinSUtil.encodeHex(bytes) + " which is of size: " + bytes.size)
  }

  def fromHex(hex: String, isCompressed: Boolean): ECPrivateKey = fromBytes(BitcoinSUtil.decodeHex(hex), isCompressed)

  /** Generates a fresh [[ECPrivateKey]] that has not been used before. */
  def apply() : ECPrivateKey = ECPrivateKey(true)

  def apply(isCompressed: Boolean) = freshPrivateKey(isCompressed)

  /** Generates a fresh [[ECPrivateKey]] that has not been used before. */
  def freshPrivateKey : ECPrivateKey = freshPrivateKey(true)

  def freshPrivateKey(isCompressed: Boolean): ECPrivateKey = {
    val secureRandom = new SecureRandom
    val generator : ECKeyPairGenerator = new ECKeyPairGenerator
    val keyGenParams : ECKeyGenerationParameters = new ECKeyGenerationParameters(CryptoParams.curve, secureRandom)
    generator.init(keyGenParams)
    val keypair : AsymmetricCipherKeyPair = generator.generateKeyPair
    val privParams: ECPrivateKeyParameters = keypair.getPrivate.asInstanceOf[ECPrivateKeyParameters]
    val priv : BigInteger = privParams.getD
    val bytes = priv.toByteArray
    ECPrivateKey.fromBytes(bytes,isCompressed)
  }
  /**
    * Takes in a base58 string and converts it into a private key.
    * Private keys starting with 'K', 'L', or 'c' correspond to compressed public keys.
    * https://en.bitcoin.it/wiki/Wallet_import_format
    *
    * @param WIF Wallet Import Format. Encoded in Base58
    * @return
    */
  def fromWIFToPrivateKey(WIF : String): ECPrivateKey = {
    val isCompressed = ECPrivateKey.isCompressed(WIF)
    val privateKeyBytes = trimFunction(WIF)
    ECPrivateKey.fromBytes(privateKeyBytes,isCompressed)
  }

  /**
    * Takes in WIF private key as a sequence of bytes and determines if it corresponds to a compressed public key.
    * If the private key corresponds to a compressed public key, the last byte should be 0x01, and
    * the WIF string will have started with K or L instead of 5 (or c instead of 9 on testnet).
    *
    * @param bytes private key in bytes
    * @return
    */
  def isCompressed(bytes : Seq[Byte]): Boolean = {
    val validCompressedBytes: Seq[Byte] =
      MainNetChainParams.base58Prefix(SecretKey) ++ TestNetChainParams.base58Prefixes(SecretKey)
    val validCompressedBytesInHex: Seq[String] = validCompressedBytes.map(byte => BitcoinSUtil.encodeHex(byte))
    val firstByteHex = BitcoinSUtil.encodeHex(bytes.head)
    if (validCompressedBytesInHex.contains(firstByteHex)) bytes(bytes.length - 5) == 0x01.toByte
    else false
  }

  def isCompressed(WIF : String) : Boolean = {
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
  private def trimFunction(WIF : String): Seq[Byte] = {
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
  def uncompressedKeyPrefixes = Seq(Some('5'),Some('9'))

  /** Returns the [[NetworkParameters]] from a serialized WIF key */
  def parseNetworkFromWIF(wif: String): Try[NetworkParameters] = {
    val decoded = Base58.decodeCheck(wif)
    decoded.map { bytes =>
      val b = bytes.head
      if (b == TestNetChainParams.base58Prefixes(SecretKey).head) {
        TestNet3
      } else if (b == MainNetChainParams.base58Prefixes(SecretKey).head) {
        MainNet
      } else throw new IllegalArgumentException("Cannot match wif private key with a network, prefix was: " + BitcoinSUtil.encodeHex(b))
    }
  }
}


