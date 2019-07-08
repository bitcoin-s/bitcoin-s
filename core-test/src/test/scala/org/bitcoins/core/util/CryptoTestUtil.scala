package org.bitcoins.core.util

import org.bitcoins.core.crypto.ECPrivateKey

trait CryptoTestUtil {
  private def privateKeyBase58 =
    "cVLwRLTvz3BxDAWkvS3yzT9pUcTCup7kQnfT2smRjvmmm1wAP6QT"

  def privateKey = ECPrivateKey.fromWIFToPrivateKey(privateKeyBase58)

}

object CryptoTestUtil extends CryptoTestUtil
