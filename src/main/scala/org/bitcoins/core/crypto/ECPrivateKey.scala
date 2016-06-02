package org.bitcoins.core.crypto

import java.math.BigInteger
import java.security.SecureRandom
import java.security.spec.ECPrivateKeySpec

import org.bitcoins.core.util.{Base58, BitcoinSUtil, Factory}
import org.spongycastle.crypto.AsymmetricCipherKeyPair
import org.spongycastle.crypto.generators.ECKeyPairGenerator
import org.spongycastle.crypto.params.{ECKeyGenerationParameters, ECPrivateKeyParameters, ECPublicKeyParameters}
import org.spongycastle.math.ec.{ECPoint, FixedPointCombMultiplier}

/**
 * Created by chris on 2/16/16.
 */
sealed trait ECPrivateKey extends BaseECKey {

  private def ecPoint = CryptoParams.curve.getCurve.decodePoint(bytes.toArray)
  /**
    * This represents the private key inside of the bouncy castle library
    * @return
    */
  private def privateKeyParams =
    new ECPrivateKeyParameters(new BigInteger(bytes.toArray), CryptoParams.curve)

  /**
   * Derives the public for a the private key
   * @return
   */
  def publicKey : ECPublicKey = {
    val pubKeyBytes : Seq[Byte] = publicKeyPoint.getEncoded(compressed)
    ECPublicKey(pubKeyBytes)
  }


  /**
    * Derives the public key ECPoint from the private key
    * https://github.com/bitcoinj/bitcoinj/blob/master/core/src/main/java/org/bitcoinj/core/ECKey.java#L452
    * @return the public key's ECPoint
    */
  private def publicKeyPoint : ECPoint = {
    val privKeyBigInteger = new BigInteger(1,bytes.toArray)
    val privKey = if (privKeyBigInteger.bitLength > CryptoParams.curve.getN.bitLength()) {
      privKeyBigInteger.mod(CryptoParams.curve.getN())
    } else privKeyBigInteger
    return new FixedPointCombMultiplier().multiply(CryptoParams.curve.getG, privKey);
  }

  override def toString = "ECPrivateKey(" + hex + ")"
}

object ECPrivateKey extends Factory[ECPrivateKey] {

  private case class ECPrivateKeyImpl(bytes : Seq[Byte]) extends ECPrivateKey

  override def fromBytes(bytes : Seq[Byte]) : ECPrivateKey = ECPrivateKeyImpl(bytes)

  /**
    * This function creates a fresh private key to use
    * @return
    */
  def apply() : ECPrivateKey = freshPrivateKey

  /**
    * This function creates a fresh private key to use
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
    * Takes in a base58 string and converts it into a private key
    * @param base58
    * @return
    */
  def fromBase58ToPrivateKey(base58 : String) : ECPrivateKey = {
    val decodedBase58 : Seq[Byte] = Base58.decode(base58)
    //Drop(1) will drop the network byte. The last 5 bytes are dropped included the checksum (4 bytes), and 0x01 byte that
    //is appended to compressed keys (which we implemented as the default option).
    val trim = decodedBase58.drop(1).dropRight(5)
    val privateKeyBytesToHex = BitcoinSUtil.encodeHex(trim)
    apply(privateKeyBytesToHex)
  }
}


