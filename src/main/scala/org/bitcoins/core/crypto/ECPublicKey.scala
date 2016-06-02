package org.bitcoins.core.crypto

import java.math.BigInteger

import org.bitcoins.core.util.{BitcoinSLogger, BitcoinSUtil, Factory}
import org.spongycastle.crypto.params.ECPublicKeyParameters
import org.spongycastle.crypto.signers.ECDSASigner

import scala.util.{Failure, Success, Try}

/**
 * Created by chris on 2/16/16.
 */
trait ECPublicKey extends BaseECKey with BitcoinSLogger {
  /**
   * The elliptic curve used by bitcoin
   * @return
   */
  private def curve = CryptoParams.curve

  /**
   * This represents this public key in the bouncy castle library
   */
  private def publicKeyParams = new ECPublicKeyParameters(curve.getCurve.decodePoint(bytes.toArray), curve)


  /**
    * Verifies that the given hash is signed by the private key corresponding to the public key
    * @param hash the hash that needs to be verified
    * @param signature the signature created by the private key corresponding to this publick ey
    * @return a boolean indicating if the signature is valid or not
    */
  def verify(hash : HashDigest, signature : ECDigitalSignature) : Boolean = verify(hash.bytes,signature)

  /**
   * Verifies if a given piece of data is signed by the private key corresponding public key
   * @param data
   * @param signature
   * @return
   */
  def verify(data : Seq[Byte], signature : ECDigitalSignature) : Boolean = {
    logger.debug("PubKey for verifying: " + BitcoinSUtil.encodeHex(bytes))
    logger.debug("Data to verify: " + BitcoinSUtil.encodeHex(data))
    logger.debug("Signature to check against data: " + signature.hex)

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
    val result : Boolean = resultTry match {
      case Success(bool) =>
        logger.info("Signature verification inside of bitcoinj did not hit an exception")
        bool
      case Failure(_) =>
        logger.warn("Signature verification inside of bitcoinj hit an exception")
        false
    }
    result
  }


  /**
   * Verifies that the given hexadecimal string is signed by the private key corresponding to this public key
 *
   * @param hex the original piece of data that was signed by the private key
   * @param signature the signature to be verified
   * @return
   */
  def verify(hex : String, signature : ECDigitalSignature) : Boolean = verify(BitcoinSUtil.decodeHex(hex),signature)

  override def toString = "ECPublicKey(" + hex + ")"
}

object ECPublicKey extends Factory[ECPublicKey] {

  private case class ECPublicKeyImpl(bytes : Seq[Byte]) extends ECPublicKey

  override def fromBytes(bytes : Seq[Byte]) : ECPublicKey = ECPublicKeyImpl(bytes)

  /**
    * Generates a fresh public key that has not been used before
    * @return
    */
  def apply() = freshPublicKey

  /**
    * Generates a fresh public key that has not been used before
    * @return
    */
  def freshPublicKey = ECPrivateKey.freshPrivateKey.publicKey
}


