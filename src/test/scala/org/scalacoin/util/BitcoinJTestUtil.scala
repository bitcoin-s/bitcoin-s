package org.scalacoin.util

import org.bitcoinj.params.TestNet3Params

/**
 * Created by chris on 3/3/16.
 */
trait BitcoinJTestUtil {

  /**
   * Returns a bitcoinj multsignature tx (NOT a p2sh tx)
   * @return
   */
  def multiSigTransaction : org.bitcoinj.core.Transaction = {
    //https://github.com/bitcoinj/bitcoinj/blob/master/core/src/test/java/org/bitcoinj/script/ScriptTest.java#L127
    val txHex = "01000000013df681ff83b43b6585fa32dd0e12b0b502e6481e04ee52ff0fdaf55a16a4ef61000000006b483045022100a84acca7906c13c5895a1314c165d33621cdcf8696145080895cbf301119b7cf0220730ff511106aa0e0a8570ff00ee57d7a6f24e30f592a10cae1deffac9e13b990012102b8d567bcd6328fd48a429f9cf4b315b859a58fd28c5088ef3cb1d98125fc4e8dffffffff02364f1c00000000001976a91439a02793b418de8ec748dd75382656453dc99bcb88ac40420f000000000017a9145780b80be32e117f675d6e0ada13ba799bf248e98700000000"
    val params = TestNet3Params.get()
    val creditingTx = new org.bitcoinj.core.Transaction(params,BitcoinSUtil.decodeHex(txHex).toArray)
    val output = creditingTx.getOutput(1)
    val spendTx = new org.bitcoinj.core.Transaction(params)
    val address = new org.bitcoinj.core.Address(params, "n3CFiCmBXVt5d3HXKQ15EFZyhPz4yj5F3H")
    val outputScript = org.bitcoinj.script.ScriptBuilder.createOutputScript(address)
    spendTx.addOutput(output.getValue(), outputScript)
    spendTx.addInput(output)
    spendTx

  }

}

object BitcoinJTestUtil extends BitcoinJTestUtil
