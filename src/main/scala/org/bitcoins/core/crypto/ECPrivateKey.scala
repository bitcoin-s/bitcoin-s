package org.bitcoins.core.crypto

import java.math.BigInteger
import java.security.SecureRandom

import org.bitcoins.core.protocol.blockchain.{MainNetChainParams, SecretKey, TestNetChainParams}
import org.bitcoins.core.util.{Base58, BitcoinSUtil, Factory}
import org.spongycastle.crypto.AsymmetricCipherKeyPair
import org.spongycastle.crypto.generators.ECKeyPairGenerator
import org.spongycastle.crypto.params.{ECKeyGenerationParameters, ECPrivateKeyParameters}
import org.spongycastle.math.ec.{ECPoint, FixedPointCombMultiplier}

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
    new ECPrivateKeyParameters(new BigInteger(bytes.toArray), CryptoParams.curve)

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

  override def toString = "ECPrivateKey(" + hex + ")"
}

object ECPrivateKey extends Factory[ECPrivateKey] {

  private case class ECPrivateKeyImpl(bytes : Seq[Byte]) extends ECPrivateKey

  override def fromBytes(bytes : Seq[Byte]) : ECPrivateKey = ECPrivateKeyImpl(bytes)

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
    apply(priv.toByteArray)
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
    val bytes = BitcoinSUtil.decodeHex(WIF)
    isCompressed(bytes)
  }

  /**
    * When decoding a WIF private key, we drop the first byte (network byte), and the last 4 bytes (checksum).
    * If the private key corresponds to a compressed public key, we drop the last byte again.
    *
    * @param WIF Wallet Import Format. Encoded in Base58
    * @return
    */
  private def trimFunction(WIF : String) : Seq[Byte] = {
    val bytes = Base58.decode(WIF)
    WIF.head match {
      case h if h == '9' || h == '5' => bytes.drop(1).dropRight(4)
      case g if isCompressed(bytes) => bytes.drop(1).dropRight(5)
      case _ => throw new IllegalArgumentException("The base58 string passed through was not a private key.")
    }
  }
}


