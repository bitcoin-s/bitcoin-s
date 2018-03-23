package org.bitcoins.core.util

import org.bitcoinj.core.DumpedPrivateKey
import org.bitcoins.core.config.TestNet3
import org.bitcoins.core.crypto.{ ECPrivateKey }

/**
 * Created by chris on 3/7/16.
 */
trait CryptoTestUtil {
  def privateKeyBase58 = "cVLwRLTvz3BxDAWkvS3yzT9pUcTCup7kQnfT2smRjvmmm1wAP6QT"
  def bitcoinjDumpedPrivateKey = new DumpedPrivateKey(BitcoinJTestUtil.params, privateKeyBase58)
  def bitcoinjPrivateKey = bitcoinjDumpedPrivateKey.getKey
  def privateKey = ECPrivateKey.fromWIFToPrivateKey(privateKeyBase58)

}

object CryptoTestUtil extends CryptoTestUtil
