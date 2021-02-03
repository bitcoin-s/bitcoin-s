package org.bitcoins.core.util

import org.bitcoins.core.crypto.ECPrivateKeyUtil

/** Created by chris on 3/7/16.
  */
trait CryptoTestUtil {
  def privateKeyBase58 = "cVLwRLTvz3BxDAWkvS3yzT9pUcTCup7kQnfT2smRjvmmm1wAP6QT"

  def privateKey = ECPrivateKeyUtil.fromWIFToPrivateKey(privateKeyBase58)

}

object CryptoTestUtil extends CryptoTestUtil
