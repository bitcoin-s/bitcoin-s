package org.bitcoins.core.util

import org.bitcoinj.core.DumpedPrivateKey
import org.bitcoins.core.config.TestNet3
import org.bitcoins.core.crypto.ECFactory

/**
 * Created by chris on 3/7/16.
 */
trait CryptoTestUtil {
  def privateKeyBase58 = "cVLwRLTvz3BxDAWkvS3yzT9pUcTCup7kQnfT2smRjvmmm1wAP6QT"
  def privateKeyBytes  = Base58.decode(privateKeyBase58)
  def privateKeyHex = BitcoinSUtil.encodeHex(privateKeyBytes)
  def bitcoinjDumpedPrivateKey = new DumpedPrivateKey(BitcoinJTestUtil.params,privateKeyBase58)
  def bitcoinjPrivateKey = bitcoinjDumpedPrivateKey.getKey
  def privateKey = ECFactory.fromBase58ToPrivateKey(privateKeyBase58)

}

object CryptoTestUtil extends CryptoTestUtil
