package org.bitcoins.core.crypto

import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.util.{Factory, BitcoinSUtil}
import java.math.BigInteger
import java.security.SecureRandom
import org.spongycastle.crypto.AsymmetricCipherKeyPair
import org.spongycastle.crypto.generators.ECKeyPairGenerator
import org.spongycastle.crypto.params.{ECKeyGenerationParameters, ECPrivateKeyParameters}

/**
 * Created by chris on 2/16/16.
 */
trait ECFactory extends Factory[BaseECKey] {

  /**
   * Creates a private key from a hex string
   * @param hex
   * @return
   */
  def privateKey(hex : String) : ECPrivateKey = ECPrivateKeyImpl(hex)

  /**
   * Creates a private key from a sequence of bytes
   * @param bytes
   * @return
   */
  def privateKey(bytes : Seq[Byte]) : ECPrivateKey = privateKey(BitcoinSUtil.encodeHex(bytes))

  /**
   * Generates a fresh ECPrivateKey
   * @return
   */
  def privateKey : ECPrivateKey = {
    val secureRandom = new SecureRandom
    val generator : ECKeyPairGenerator = new ECKeyPairGenerator
    val keyGenParams : ECKeyGenerationParameters = new ECKeyGenerationParameters(CryptoParams.curve, secureRandom)
    generator.init(keyGenParams)
    val keypair : AsymmetricCipherKeyPair = generator.generateKeyPair
    val privParams: ECPrivateKeyParameters = keypair.getPrivate.asInstanceOf[ECPrivateKeyParameters]
    val priv : BigInteger = privParams.getD
    privateKey(priv.toByteArray)
  }

  /**
   * Creates a public key from a hex string
   * @param hex
   * @return
   */
  def publicKey(hex : String) : ECPublicKey =  {
    if (hex == "") ECPublicKeyImpl("00")
    else ECPublicKeyImpl(hex)
  }

  /**
   * Creates a public key from a sequence of bytes
   * @param bytes
   * @return
   */
  def publicKey(bytes : Seq[Byte]) : ECPublicKey = publicKey(BitcoinSUtil.encodeHex(bytes))

  /**
   * Generates a fresh public key
   * @return
   */
  def publicKey = privateKey.publicKey


  /**
   * Creates a digital signature from the given hex string
   * @param hex
   * @return
   */
  def digitalSignature(hex : String) : ECDigitalSignature = digitalSignature(BitcoinSUtil.decodeHex(hex))

  /**
   * Creates a digital signature from the given sequence of bytes
   * @param bytes
   * @return
   */
  def digitalSignature(bytes : Seq[Byte]) : ECDigitalSignature = {
    //this represents the empty signature
    if (bytes.size == 1 && bytes.head == 0x0) EmptyDigitalSignature
    else if (bytes.size == 0) EmptyDigitalSignature
    else ECDigitalSignatureImpl(bytes)
  }

  /**
    * Takes in the r and s component of a digital signature and gives back a ECDigital signature object
    * @param r the r component of the digital signature
    * @param s the s component of the digital signature
    * @return
    */
  def digitalSignature(r : BigInteger, s : BigInteger) : ECDigitalSignature = {
    ???
  }


  /**
   * Creates a private key from a hex string
   * @param hex
   * @return
   */
  override def fromHex(hex : String) : BaseECKey = privateKey(hex)

  /**
   * Creates a private key from a byte array
   * @param bytes
   * @return
   */
  override def fromBytes(bytes : Seq[Byte]) : BaseECKey = privateKey(bytes)


  /**
   * Takes in a base58 string and converts it into a private key
   * @param base58
   * @return
   */
  def fromBase58ToPrivateKey(base58 : String, network : NetworkParameters) : ECPrivateKey = {
    ???
/*    val bytes = BitcoinSUtil.decodeBase58(base58)
    privateKey(bytes)*/
  }

}

object ECFactory extends ECFactory
