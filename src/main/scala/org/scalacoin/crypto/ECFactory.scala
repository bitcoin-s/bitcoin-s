package org.scalacoin.crypto

import org.scalacoin.util.{Factory, BitcoinSUtil}

/**
 * Created by chris on 2/16/16.
 */
trait ECFactory extends Factory[BaseECKey] {

  def privateKey(hex : String) : ECPrivateKey = privateKey(BitcoinSUtil.decodeHex(hex))

  def privateKey(bytes : Seq[Byte]) : ECPrivateKey = ECPrivateKeyImpl(bytes)

  def privateKey(bytes : List[Byte]) : ECPrivateKey = privateKey(bytes.toSeq)

  def publicKey(hex : String) : ECPublicKey = publicKey(BitcoinSUtil.decodeHex(hex))

  def publicKey(bytes : Seq[Byte]) : ECPublicKey = ECPublicKeyImpl(bytes)

  def publicKey(bytes : List[Byte]) : ECPublicKey = publicKey(bytes.toSeq)


  def digitalSignature(hex : String) : ECDigitalSignature = ECDigitalSignatureImpl(BitcoinSUtil.decodeHex(hex))

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

}

object ECFactory extends ECFactory
