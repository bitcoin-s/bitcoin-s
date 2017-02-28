package org.bitcoins.core.crypto

import java.math.BigInteger

import org.bitcoin.NativeSecp256k1
import org.bitcoins.core.config.NetworkParameters
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
   * Signs a given sequence of bytes with the signingKey
   * @param dataToSign the bytes to be signed
   * @param signingKey the key to sign the bytes with
   * @return the digital signature
   */
  private def sign(dataToSign : Seq[Byte], signingKey : BaseECKey): ECDigitalSignature = {
    require(dataToSign.length == 32 && signingKey.bytes.length <= 32)
    val signature = NativeSecp256k1.sign(dataToSign.toArray, signingKey.bytes.toArray)
    ECDigitalSignature(signature)
  }

  def sign(hash: DoubleSha256Digest, signingKey: BaseECKey): ECDigitalSignature = sign(hash.bytes,signingKey)

  def sign(hash: DoubleSha256Digest): ECDigitalSignature = sign(hash,this)

  @deprecated("Deprecated in favor of signing algorithm inside of secp256k1", "2/20/2017")
  private def oldSign(dataToSign: Seq[Byte], signingKey: BaseECKey): ECDigitalSignature = {
    val signer: ECDSASigner = new ECDSASigner(new HMacDSAKCalculator(new SHA256Digest()))
    val privKey: ECPrivateKeyParameters = new ECPrivateKeyParameters(
      new BigInteger(1,signingKey.bytes.toArray), CryptoParams.curve)
    signer.init(true, privKey)
    val components : Array[BigInteger] = signer.generateSignature(dataToSign.toArray)
    val (r,s) = (components(0),components(1))
    val signature = ECDigitalSignature(r,s)
    //make sure the signature follows BIP62's low-s value
    //https://github.com/bitcoin/bips/blob/master/bip-0062.mediawiki#Low_S_values_in_signatures
    //bitcoinj implementation
    //https://github.com/bitcoinj/bitcoinj/blob/1e66b9a8e38d9ad425507bf5f34d64c5d3d23bb8/core/src/main/java/org/bitcoinj/core/ECKey.java#L551
    val signatureLowS = DERSignatureUtil.lowS(signature)
    require(signatureLowS.isDEREncoded, "We must create DER encoded signatures when signing a piece of data, got: " + signatureLowS)
    signatureLowS
  }
}

object BaseECKey extends Factory[BaseECKey] {

  /** Creates a [[ECPrivateKey]] from a hex string. */
  override def fromHex(hex : String) : BaseECKey = ECPrivateKey(hex)

  /** Creates a [[ECPrivateKey]] from a sequence of bytes. */
  override def fromBytes(bytes : Seq[Byte]) : BaseECKey = ECPrivateKey(bytes)
}