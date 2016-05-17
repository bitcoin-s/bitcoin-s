package org.bitcoins.core.crypto

import java.math.BigInteger
import java.security.spec.ECPrivateKeySpec

import org.spongycastle.crypto.params.{ECPrivateKeyParameters, ECPublicKeyParameters}

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
    ???
/*    privateKeyParams.
    val bitcoinjECKey : org.bitcoinj.core.ECKey = ECKey.fromPrivate(bytes.toArray)
    ECFactory.publicKey(bitcoinjECKey.getPubKey)*/
  }

}

sealed case class ECPrivateKeyImpl(hex : String) extends ECPrivateKey
