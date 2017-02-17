package org.bitcoins.core.crypto

import java.math.BigInteger

import org.bitcoin.NativeSecp256k1
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.util.{BitcoinSLogger, BitcoinSUtil, Factory}
import org.spongycastle.crypto.params.ECPublicKeyParameters
import org.spongycastle.crypto.signers.ECDSASigner

import scala.util.{Failure, Success, Try}

/**
 * Created by chris on 2/16/16.
 */
trait ECPublicKey extends BaseECKey with BitcoinSLogger {
  /** The elliptic curve used by bitcoin. */
  private def curve = CryptoParams.curve

  /** This represents this public key in the bouncy castle library */
  private def publicKeyParams = new ECPublicKeyParameters(curve.getCurve.decodePoint(bytes.toArray), curve)

  def verify(hash : HashDigest, signature : ECDigitalSignature) : Boolean = verify(hash.bytes, signature)

  /** Verifies if a given piece of data is signed by the [[ECPrivateKey]]'s corresponding [[ECPublicKey]]. */
  def verify(data : Seq[Byte], signature : ECDigitalSignature): Boolean = {
    logger.debug("PubKey for verifying: " + BitcoinSUtil.encodeHex(bytes))
    logger.debug("Data to verify: " + BitcoinSUtil.encodeHex(data))
    logger.debug("Signature to check against data: " + signature.hex)
    NativeSecp256k1.verify(data.toArray, signature.bytes.toArray, bytes.toArray)
  }

  def verify(hex : String, signature : ECDigitalSignature) : Boolean = verify(BitcoinSUtil.decodeHex(hex),signature)

  override def toString = "ECPublicKey(" + hex + ")"
}

object ECPublicKey extends Factory[ECPublicKey] {

  private case class ECPublicKeyImpl(bytes : Seq[Byte]) extends ECPublicKey

  override def fromBytes(bytes : Seq[Byte]) : ECPublicKey = ECPublicKeyImpl(bytes)

  def apply() = freshPublicKey

  /** Generates a fresh [[ECPublicKey]] that has not been used before. */
  def freshPublicKey = ECPrivateKey.freshPrivateKey.publicKey
}


