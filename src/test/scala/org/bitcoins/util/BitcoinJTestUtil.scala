package org.bitcoins.util

import java.util

import com.google.common.collect.ImmutableList
import org.bitcoinj.core.{ECKey, DumpedPrivateKey}
import org.bitcoinj.core.Transaction.SigHash
import org.bitcoinj.crypto.TransactionSignature
import org.bitcoinj.params.TestNet3Params
import org.bitcoinj.script.{Script, ScriptBuilder}
import org.slf4j.LoggerFactory

/**
 * Created by chris on 3/3/16.
 */
trait BitcoinJTestUtil {

  private def logger = LoggerFactory.getLogger(this.getClass())
  def params = TestNet3Params.get();
  def key1 = new DumpedPrivateKey(params, "cVLwRLTvz3BxDAWkvS3yzT9pUcTCup7kQnfT2smRjvmmm1wAP6QT").getKey();
  def key2 = new DumpedPrivateKey(params, "cTine92s8GLpVqvebi8rYce3FrUYq78ZGQffBYCS1HmDPJdSTxUo").getKey();
  def key3 = new DumpedPrivateKey(params, "cVHwXSPRZmL9adctwBwmn4oTZdZMbaCsR5XF6VznqMgcvt1FDDxg").getKey();
  def multiSigScript : org.bitcoinj.script.Script = ScriptBuilder.createMultiSigOutputScript(2,
    util.Arrays.asList(BitcoinJTestUtil.key1, key2, key3));

  /**
   * Returns a bitcoinj multsignature tx (NOT a p2sh tx)
   * @return
   */
  def multiSigTransaction : org.bitcoinj.core.Transaction = {
    //https://github.com/bitcoinj/bitcoinj/blob/master/core/src/test/java/org/bitcoinj/script/ScriptTest.java#L127
    val txHex = "01000000013df681ff83b43b6585fa32dd0e12b0b502e6481e04ee52ff0fdaf55a16a4ef61000000006b483045022100a84acca7906c13c5895a1314c165d33621cdcf8696145080895cbf301119b7cf0220730ff511106aa0e0a8570ff00ee57d7a6f24e30f592a10cae1deffac9e13b990012102b8d567bcd6328fd48a429f9cf4b315b859a58fd28c5088ef3cb1d98125fc4e8dffffffff02364f1c00000000001976a91439a02793b418de8ec748dd75382656453dc99bcb88ac40420f000000000017a9145780b80be32e117f675d6e0ada13ba799bf248e98700000000"
    val creditingTx = new org.bitcoinj.core.Transaction(params,BitcoinSUtil.decodeHex(txHex).toArray)
    val output = creditingTx.getOutput(1)
    val spendTx = new org.bitcoinj.core.Transaction(params)
    val address = new org.bitcoinj.core.Address(params, "n3CFiCmBXVt5d3HXKQ15EFZyhPz4yj5F3H")
    val outputScript = org.bitcoinj.script.ScriptBuilder.createOutputScript(address)
    spendTx.addOutput(output.getValue(), outputScript)
    spendTx.addInput(output)
    //spendTx.getInput(0).getScriptSig.correctlySpends(spendTx,0,outputScript)
    spendTx
  }

  /**
   * Signs the multisig transaction and returns the signed tx
   * @return
   */
  def signedMultiSigTransaction : (org.bitcoinj.core.Transaction,Int,Script) = {
    //https://github.com/bitcoinj/bitcoinj/blob/master/core/src/test/java/org/bitcoinj/script/ScriptTest.java#L127
    val txHex = "01000000013df681ff83b43b6585fa32dd0e12b0b502e6481e04ee52ff0fdaf55a16a4ef61000000006b483045022100a84acca7906c13c5895a1314c165d33621cdcf8696145080895cbf301119b7cf0220730ff511106aa0e0a8570ff00ee57d7a6f24e30f592a10cae1deffac9e13b990012102b8d567bcd6328fd48a429f9cf4b315b859a58fd28c5088ef3cb1d98125fc4e8dffffffff02364f1c00000000001976a91439a02793b418de8ec748dd75382656453dc99bcb88ac40420f000000000017a9145780b80be32e117f675d6e0ada13ba799bf248e98700000000"
    val multisigScript = ScriptBuilder.createMultiSigOutputScript(2, util.Arrays.asList(key1, key2, key3));
    logger.info("multiSigScript: " + multisigScript)
    val creditingTx = new org.bitcoinj.core.Transaction(params,BitcoinSUtil.decodeHex(txHex).toArray)
    val output = creditingTx.getOutput(1)
    val spendTx = new org.bitcoinj.core.Transaction(params)
    val address = new org.bitcoinj.core.Address(params, "n3CFiCmBXVt5d3HXKQ15EFZyhPz4yj5F3H")
    val outputScript : Script = org.bitcoinj.script.ScriptBuilder.createOutputScript(address)
    spendTx.addOutput(output.getValue(), outputScript)
    spendTx.addInput(output)
    val inputIndex = 0

    val sighash = spendTx.hashForSignature(inputIndex, multisigScript, SigHash.ALL, false)
    val party1Signature : ECKey.ECDSASignature = key1.sign(sighash);
    val party2Signature : ECKey.ECDSASignature = key2.sign(sighash);
    val party1TransactionSignature : TransactionSignature = new TransactionSignature(party1Signature, SigHash.ALL, false);
    val party2TransactionSignature : TransactionSignature  = new TransactionSignature(party2Signature, SigHash.ALL, false);

    val inputScript = ScriptBuilder.createMultiSigInputScript(ImmutableList.of(party1TransactionSignature,
      party2TransactionSignature));

    spendTx.getInput(inputIndex).setScriptSig(inputScript)

    //make sure the sig is right, throws exception if it is wrong
    inputScript.correctlySpends(spendTx,0,multisigScript)
    (spendTx,inputIndex,multisigScript)
  }

  /**
   * Returns a p2sh transaction with its corresponding crediting output
   * @return
   */
  def p2shTransactionWithSpendingInputAndCreditingOutput : (org.bitcoinj.core.Transaction,
    org.bitcoinj.core.TransactionInput, Int, org.bitcoinj.core.TransactionOutput) = {
    val rawCreditingTx = TestUtil.rawP2SH2Of2CreditingTx
    val creditingTx = new org.bitcoinj.core.Transaction(params, BitcoinSUtil.decodeHex(rawCreditingTx).toArray)

    val spendingRawTx = TestUtil.rawP2SH2Of2Tx
    val spendingTx = new org.bitcoinj.core.Transaction(params, BitcoinSUtil.decodeHex(spendingRawTx).toArray)
    val inputIndex = 0
    val input = spendingTx.getInput(inputIndex)
    val output = creditingTx.getOutput(input.getOutpoint.getIndex)
    (spendingTx,input,inputIndex,output)

  }
}

object BitcoinJTestUtil extends BitcoinJTestUtil
