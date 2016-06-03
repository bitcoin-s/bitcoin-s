package org.bitcoins.core.crypto

import java.math.BigInteger

import org.bitcoins.core.util._
import org.bitcoins.core.util.{BitcoinSLogger, BitcoinSUtil}
import org.spongycastle.crypto.digests.SHA256Digest
import org.spongycastle.crypto.params.ECPrivateKeyParameters
import org.spongycastle.crypto.signers.{ECDSASigner, HMacDSAKCalculator}

/**
 * Created by chris on 2/16/16.
 */
trait BaseECKey extends BitcoinSLogger {

  def hex : String = BitcoinSUtil.encodeHex(bytes)

  def bytes : Seq[Byte]

  /**
    * Use compressed keys by default
    * @return
    */
  def compressed : Boolean = true

  /**
   * Signs a given sequence of bytes with the signingKey
   * @param dataToSign the bytes to be signed
   * @param signingKey the key to sign the bytes with
   * @return the digital signature
   */
  def sign(dataToSign : Seq[Byte], signingKey : BaseECKey) : ECDigitalSignature = {
    val signer: ECDSASigner = new ECDSASigner(new HMacDSAKCalculator(new SHA256Digest()))
    val privKey: ECPrivateKeyParameters = new ECPrivateKeyParameters(
      new BigInteger(signingKey.bytes.toArray), CryptoParams.curve)
    signer.init(true, privKey)
    val components : Array[BigInteger] = signer.generateSignature(dataToSign.toArray)
    val (r,s) = (components(0),components(1))
    ECDigitalSignature(r,s)
  }

  def sign(hex : String, signingKey : BaseECKey) : ECDigitalSignature = sign(BitcoinSUtil.decodeHex(hex),signingKey)

  def sign(hex : String) : ECDigitalSignature = sign(hex,this)

  def sign(bytes : Seq[Byte]) : ECDigitalSignature = sign(bytes,this)

}

object BaseECKey extends Factory[BaseECKey] {

  /**
    * Creates a private key from a hex string
    * @param hex
    * @return
    */
  override def fromHex(hex : String) : BaseECKey = ECPrivateKey(hex)

  /**
    * Creates a private key from a byte array
    * @param bytes
    * @return
    */
  override def fromBytes(bytes : Seq[Byte]) : BaseECKey = ECPrivateKey(bytes)
}