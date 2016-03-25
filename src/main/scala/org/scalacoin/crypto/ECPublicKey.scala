package org.scalacoin.crypto

import org.scalacoin.util.{BitcoinSLogger, BitcoinSUtil}

import scala.util.{Failure, Success, Try}

/**
 * Created by chris on 2/16/16.
 */
trait ECPublicKey extends BaseECKey with BitcoinSLogger {
  import org.bitcoinj.core.ECKey



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
    val bitcoinjKey = ECKey.fromPublicOnly(bytes.toArray)

    if (signature.isEmpty) bitcoinjKey.verify(data.toArray,emptySignature.encodeToDER())
    else {
      val resultTry = Try(bitcoinjKey.verify(data.toArray, signature.bytes.toArray))
      val result : Boolean = resultTry match  {
        case Success(bool) => bool
        case Failure(_) => false
      }
      result
    }

  }
}

case class ECPublicKeyImpl(hex : String) extends ECPublicKey
