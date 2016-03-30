package org.scalacoin.crypto

import java.math.BigInteger

import org.scalacoin.util.{BitcoinSLogger, BitcoinSUtil}
import org.spongycastle.crypto.params.ECPublicKeyParameters
import org.spongycastle.crypto.signers.ECDSASigner

import scala.util.{Failure, Success, Try}

/**
 * Created by chris on 2/16/16.
 */
trait ECPublicKey extends BaseECKey with BitcoinSLogger {
  import org.bitcoinj.core.ECKey


  /**
   * The elliptic curve used by bitcoin
   * @return
   */
  private def curve = CryptoParams.curve

  /**
   * This represents this public key in the bouncy castle library
   */
  private def publicKeyParams = new ECPublicKeyParameters(curve.getCurve.decodePoint(bytes.toArray), curve)

  private def emptySignature = new org.bitcoinj.core.ECKey.ECDSASignature(java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(0))
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


    val signer = new ECDSASigner
    signer.init(false,publicKeyParams)
    signature match {
      case EmptyDigitalSignature =>  signer.verifySignature(data.toArray,java.math.BigInteger.valueOf(0),java.math.BigInteger.valueOf(0))
      case sig : ECDigitalSignature =>
        logger.debug("Public key bytes: " + BitcoinSUtil.encodeHex(bytes))
        val rBigInteger : BigInteger = new BigInteger(signature.r.toString())
        val sBigInteger : BigInteger = new BigInteger(signature.s.toString())
        val resultTry = Try(signer.verifySignature(data.toArray, rBigInteger, sBigInteger))
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
  }
}

case class ECPublicKeyImpl(hex : String) extends ECPublicKey
