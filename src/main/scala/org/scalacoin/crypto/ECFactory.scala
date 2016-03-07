package org.scalacoin.crypto

import org.bitcoinj.core.DumpedPrivateKey
import org.scalacoin.config.NetworkParameters
import org.scalacoin.util.{Factory, BitcoinSUtil}

/**
 * Created by chris on 2/16/16.
 */
trait ECFactory extends Factory[BaseECKey] {

  /**
   * Creates a private key from a hex string
   * @param hex
   * @return
   */
  def privateKey(hex : String) : ECPrivateKey = privateKey(BitcoinSUtil.decodeHex(hex))

  /**
   * Creates a private key from a sequence of bytes
   * @param bytes
   * @return
   */
  def privateKey(bytes : Seq[Byte]) : ECPrivateKey = ECPrivateKeyImpl(bytes)

  /**
   * Generates a fresh ECPrivateKey
   * @return
   */
  def privateKey : ECPrivateKey = {
    val bitcoinjKey = new org.bitcoinj.core.ECKey
    privateKey(bitcoinjKey.getPrivKeyBytes)
  }

  /**
   * Creates a public key from a hex string
   * @param hex
   * @return
   */
  def publicKey(hex : String) : ECPublicKey = publicKey(BitcoinSUtil.decodeHex(hex))

  /**
   * Creates a public key from a sequence of bytes
   * @param bytes
   * @return
   */
  def publicKey(bytes : Seq[Byte]) : ECPublicKey = ECPublicKeyImpl(bytes)

  /**
   * Generates a fresh public key
   * @return
   */
  def publicKey = {
    privateKey.publicKey
  }

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
  def digitalSignature(bytes : Seq[Byte]) : ECDigitalSignature = ECDigitalSignatureImpl(bytes)

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
    val bitcoinJDumpedPrivKey = new DumpedPrivateKey(network.network,base58)

    privateKey(bitcoinJDumpedPrivKey.getKey.getPrivKeyBytes)
  }

}

object ECFactory extends ECFactory
