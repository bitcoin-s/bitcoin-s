package org.bitcoins.core.crypto

import java.math.BigInteger
import java.security.spec.ECPrivateKeySpec

import org.spongycastle.crypto.params.{ECPrivateKeyParameters, ECPublicKeyParameters}
import org.spongycastle.math.ec.{ECPoint, FixedPointCombMultiplier}

/**
 * Created by chris on 2/16/16.
 */
sealed trait ECPrivateKey extends BaseECKey {

  private def ecPoint = CryptoParams.curve.getCurve.decodePoint(bytes.toArray)
  /**
    * This represents the private key inside of the bouncy castle library
    * @return
    */
  private def privateKeyParams =
    new ECPrivateKeyParameters(new BigInteger(bytes.toArray), CryptoParams.curve)

  /**
   * Derives the public for a the private key
   * @return
   */
  def publicKey : ECPublicKey = {
    val pubKeyBytes : Seq[Byte] = publicKeyPoint.getEncoded(compressed)
    ECFactory.publicKey(pubKeyBytes)
  }


  /**
    * Derives the public key ECPoint from the private key
    * https://github.com/bitcoinj/bitcoinj/blob/master/core/src/main/java/org/bitcoinj/core/ECKey.java#L452
    * @return the public key's ECPoint
    */
  private def publicKeyPoint : ECPoint = {
    val privKeyBigInteger = new BigInteger(bytes.toArray)
    val privKey = if (privKeyBigInteger.bitLength > CryptoParams.curve.getN.bitLength()) {
      privKeyBigInteger.mod(CryptoParams.curve.getN())
    } else privKeyBigInteger
    return new FixedPointCombMultiplier().multiply(CryptoParams.curve.getG, privKey);
  }

}

sealed case class ECPrivateKeyImpl(hex : String) extends ECPrivateKey
