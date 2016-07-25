package org.bitcoins.core.crypto

import java.math.BigInteger
import java.security.SecureRandom

import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.protocol.blockchain.{MainNetChainParams, SecretKey, TestNetChainParams}
import org.bitcoins.core.util._
import org.spongycastle.crypto.AsymmetricCipherKeyPair
import org.spongycastle.crypto.generators.ECKeyPairGenerator
import org.spongycastle.crypto.params.{ECKeyGenerationParameters, ECPrivateKeyParameters}
import org.spongycastle.math.ec.{ECPoint, FixedPointCombMultiplier}

import scala.util.{Failure, Success}

/**
 * Created by chris on 2/16/16.
 */
sealed trait ECPrivateKey extends BaseECKey {

  private def ecPoint = CryptoParams.curve.getCurve.decodePoint(bytes.toArray)
  /**
    * This represents the private key inside of the bouncy castle library
    *
    * @return
    */
  private def privateKeyParams =
    new ECPrivateKeyParameters(new BigInteger(1,bytes.toArray), CryptoParams.curve)

  /**
   * Derives the public for a the private key
    *
    * @return
   */
  def publicKey : ECPublicKey = {
    val pubKeyBytes : Seq[Byte] = publicKeyPoint.getEncoded(compressed)
    ECPublicKey(pubKeyBytes)
  }


  /**
    * Derives the public key ECPoint from the private key
    * https://github.com/bitcoinj/bitcoinj/blob/master/core/src/main/java/org/bitcoinj/core/ECKey.java#L452
    *
    * @return the public key's ECPoint
    */
  private def publicKeyPoint : ECPoint = {
    val privKeyBigInteger = new BigInteger(1,bytes.toArray)
    val privKey = if (privKeyBigInteger.bitLength > CryptoParams.curve.getN.bitLength()) {
      privKeyBigInteger.mod(CryptoParams.curve.getN())
    } else privKeyBigInteger
    new FixedPointCombMultiplier().multiply(CryptoParams.curve.getG, privKey)
  }

  /**
    * Converts a [[ECPrivateKey]] to WIF
    * https://en.bitcoin.it/wiki/Wallet_import_format
    * @return
    */
  def toWIF(network: NetworkParameters, compressed: Boolean = true): String = {
    val networkByte = network.privateKey
    val fullBytes = networkByte +: bytes
    val hash = CryptoUtil.doubleSHA256(fullBytes)
    val checksum = hash.bytes.take(4)
    val encodedPrivKey = fullBytes ++ checksum
    Base58.encode(encodedPrivKey)
  }

  override def toString = "ECPrivateKey(" + hex + ")"
}

object ECPrivateKey extends Factory[ECPrivateKey] with BitcoinSLogger {

  private case class ECPrivateKeyImpl(bytes : Seq[Byte]) extends ECPrivateKey

  override def fromBytes(bytes : Seq[Byte]) : ECPrivateKey = {
    if (bytes.size <= 32) ECPrivateKeyImpl(bytes)
    //this is for the case when java serialies a BigInteger to 33 bytes to hold the signed num representation
    else if (bytes.size == 33) ECPrivateKeyImpl(bytes.slice(1,33))
    else throw new IllegalArgumentException("Private keys cannot be greater than 33 bytes in size, got: " +
      BitcoinSUtil.encodeHex(bytes) + " which is of size: " + bytes.size)
  }

  /**
    * This function creates a fresh private key to use
    *
    * @return
    */
  def apply() : ECPrivateKey = freshPrivateKey

  /**
    * This function creates a fresh private key to use
    *
    * @return
    */
  def freshPrivateKey : ECPrivateKey = {
    val secureRandom = new SecureRandom
    val generator : ECKeyPairGenerator = new ECKeyPairGenerator
    val keyGenParams : ECKeyGenerationParameters = new ECKeyGenerationParameters(CryptoParams.curve, secureRandom)
    generator.init(keyGenParams)
    val keypair : AsymmetricCipherKeyPair = generator.generateKeyPair
    val privParams: ECPrivateKeyParameters = keypair.getPrivate.asInstanceOf[ECPrivateKeyParameters]
    val priv : BigInteger = privParams.getD
    val bytes = priv.toByteArray
    ECPrivateKey(bytes)
  }

  /**
    * Takes in a base58 string and converts it into a private key.
    * Private keys starting with 'K', 'L', or 'c' correspond to compressed public keys.
    * https://en.bitcoin.it/wiki/Wallet_import_format
    *
    * @param WIF Wallet Import Format. Encoded in Base58
    * @return
    */
  def fromWIFToPrivateKey(WIF : String) : ECPrivateKey = {
    val trimmedBytes = trimFunction(WIF)
    val privateKeyBytesToHex = BitcoinSUtil.encodeHex(trimmedBytes)
    ECPrivateKey(privateKeyBytesToHex)
  }

  /**
    * Takes in WIF private key as a sequence of bytes and determines if it corresponds to a compressed public key.
    * If the private key corresponds to a compressed public key, the last byte should be 0x01, and
    * the WIF string will have started with K or L instead of 5 (or c instead of 9 on testnet).
    *
    * @param bytes private key in bytes
    * @return
    */
  def isCompressed(bytes : Seq[Byte]) : Boolean = {
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
  private def trimFunction(WIF : String) : Seq[Byte] = {
    val bytesChecked = Base58.decodeCheck(WIF)
    val uncompressedKeyPrefixes = Seq(Some('5'),Some('9'))
    //see https://en.bitcoin.it/wiki/List_of_address_prefixes
    //for where '5' and '9' come from
    bytesChecked match {
      case Success(bytes) if uncompressedKeyPrefixes.contains(WIF.headOption) => bytes.tail
      case Success(bytes) if isCompressed(WIF) => bytes.tail.dropRight(1)
      case Success(bytes) => bytes.tail
      case Failure(exception) => throw exception
    }
  }

}


