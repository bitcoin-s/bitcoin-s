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
sealed trait ECPublicKey extends BaseECKey with BitcoinSLogger {


  def verify(hash : HashDigest, signature : ECDigitalSignature) : Boolean = verify(hash.bytes, signature)

  /** Verifies if a given piece of data is signed by the [[ECPrivateKey]]'s corresponding [[ECPublicKey]]. */
  def verify(data : Seq[Byte], signature : ECDigitalSignature): Boolean = {
    logger.debug("PubKey for verifying: " + BitcoinSUtil.encodeHex(bytes))
    logger.debug("Data to verify: " + BitcoinSUtil.encodeHex(data))
    logger.debug("Signature to check against data: " + signature.hex)
    val result = NativeSecp256k1.verify(data.toArray, signature.bytes.toArray, bytes.toArray)
    if (!result) {
      //if signature verification fails with libsecp256k1 we need to use our old
      //verification function from spongy castle, this is needed because early blockchain
      //transactions can have weird non strict der encoded digital signatures
      //bitcoin core implements this functionality here:
      //https://github.com/bitcoin/bitcoin/blob/master/src/pubkey.cpp#L16-L165
      //TODO: Implement functionality in Bitcoin Core linked above
      oldVerify(data,signature)
    } else result
  }

  def verify(hex : String, signature : ECDigitalSignature) : Boolean = verify(BitcoinSUtil.decodeHex(hex),signature)

  override def toString = "ECPublicKey(" + hex + ")"

  @deprecated("Deprecated in favor of using verify functionality inside of secp256k1", "2/20/2017")
  private def oldVerify(data: Seq[Byte], signature: ECDigitalSignature): Boolean = {
    /** The elliptic curve used by bitcoin. */
    def curve = CryptoParams.curve
    /** This represents this public key in the bouncy castle library */
    def publicKeyParams = new ECPublicKeyParameters(curve.getCurve.decodePoint(bytes.toArray), curve)

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
    resultTry.getOrElse(false)
  }

  /** Checks if the [[ECPublicKey]] is compressed */
  def isCompressed: Boolean = bytes.size == 33

  /** Checks if the [[ECPublicKey]] is valid according to secp256k1 */
  def isFullyValid = ECPublicKey.isFullyValid(bytes)
}

object ECPublicKey extends Factory[ECPublicKey] {

  private case class ECPublicKeyImpl(bytes : Seq[Byte]) extends ECPublicKey {
    //unfortunately we cannot place ANY invariants here
    //because of old transactions on the blockchain that have weirdly formatted public keys. Look at example in script_tests.json
    //https://github.com/bitcoin/bitcoin/blob/master/src/test/data/script_tests.json#L457
    //bitcoin core only checks CPubKey::IsValid()
    //this means we can have public keys with only one byte i.e. 0x00 or no bytes.
    //Eventually we would like this to be CPubKey::IsFullyValid() but since we are remaining backwards compatible
    //we cannot do this. If there ever is a hard fork this would be a good thing to add.
  }

  override def fromBytes(bytes : Seq[Byte]) : ECPublicKey = ECPublicKeyImpl(bytes)

  def apply() = freshPublicKey

  /** Generates a fresh [[ECPublicKey]] that has not been used before. */
  def freshPublicKey = ECPrivateKey.freshPrivateKey.publicKey


  /** Checks if the public key is valid according to secp256k1
    * Mimics this function in bitcoin core
    * [[https://github.com/bitcoin/bitcoin/blob/27765b6403cece54320374b37afb01a0cfe571c3/src/pubkey.cpp#L207-L212]]
    */
  def isFullyValid(bytes: Seq[Byte]): Boolean = Try(NativeSecp256k1.isValidPubKey(bytes.toArray)).isSuccess

}


