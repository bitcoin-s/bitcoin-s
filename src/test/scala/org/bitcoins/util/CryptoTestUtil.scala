package org.bitcoins.util

import org.bitcoinj.core.DumpedPrivateKey
import org.bitcoins.config.TestNet3
import org.bitcoins.crypto.ECFactory

/**
 * Created by chris on 3/7/16.
 */
trait CryptoTestUtil {
  def privateKeyBase58 = "cVLwRLTvz3BxDAWkvS3yzT9pUcTCup7kQnfT2smRjvmmm1wAP6QT"
  def privateKeyBytes  = BitcoinSUtil.decodeBase58(privateKeyBase58)
  def privateKeyHex = BitcoinSUtil.encodeHex(privateKeyBytes)
  def bitcoinjDumpedPrivateKey = new DumpedPrivateKey(BitcoinJTestUtil.params,privateKeyBase58)
  def bitcoinjPrivateKey = bitcoinjDumpedPrivateKey.getKey
  def privateKey = ECFactory.fromBase58ToPrivateKey(privateKeyBase58,TestNet3)

}

object CryptoTestUtil extends CryptoTestUtil
