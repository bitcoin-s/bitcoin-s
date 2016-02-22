package org.scalacoin.crypto

import java.util

import org.bitcoinj.core.DumpedPrivateKey
import org.bitcoinj.core.Transaction.SigHash
import org.bitcoinj.params.TestNet3Params
import org.bitcoinj.script.ScriptBuilder
import org.scalacoin.protocol.script.{ScriptPubKey, ScriptPubKeyFactory}
import org.scalacoin.script.constant.{OP_2, OP_0, OP_1, ScriptToken}
import org.scalacoin.script.crypto.OP_CODESEPARATOR
import org.scalacoin.util.{ScalacoinUtil, TestUtil}
import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 2/19/16.
 */
class TransactionSignatureSerializerTest extends FlatSpec with MustMatchers {


  "TransactionSignatureSerializer" must "serialize a given script signature without OP_CODESEPARATORS" in {
    val txSerializer = new BaseTransactionSignatureSerializer(TestUtil.transaction)
    val scriptPubKey = TestUtil.scriptPubKey.asm
    val expectedScript = txSerializer.removeOpCodeSeparators(scriptPubKey)
    txSerializer.serializeScriptCode(scriptPubKey) must be (expectedScript)
  }

  it must "serialize a given script with only OP_CODESEPARATORs" in {
    val txSerializer = new BaseTransactionSignatureSerializer(TestUtil.transaction)
    val script = List(OP_CODESEPARATOR)
    txSerializer.serializeScriptCode(script) must be ("00")
  }

  it must "serialize a given script with mixed in OP_CODESEPARATORs" in {
    val txSerializer = new BaseTransactionSignatureSerializer(TestUtil.transaction)
    val script = List(OP_CODESEPARATOR, OP_1, OP_CODESEPARATOR, OP_0, OP_CODESEPARATOR, OP_2)
    txSerializer.serializeScriptCode(script) must be ("03510052")
  }


  it must "hash a multisignature SIGHASH_ALL correctly" in {



  }


  /**
   * Mimics a test case inside of bitcoinj
   * https://github.com/bitcoinj/bitcoinj/blob/c9cce479624bfd4d6f94824f9da885e24d18ea7c/core/src/test/java/org/bitcoinj/script/ScriptTest.java#L127
   * hashes a bitcoinj tx for a signature
   * @return
   */
  private def createBitcoinjMultiSigScriptHashForSig : String = {
    val params = TestNet3Params.get();
    val spendTx = new org.bitcoinj.core.Transaction(params);
    val key1 = new DumpedPrivateKey(params, "cVLwRLTvz3BxDAWkvS3yzT9pUcTCup7kQnfT2smRjvmmm1wAP6QT").getKey();
    val key2 = new DumpedPrivateKey(params, "cTine92s8GLpVqvebi8rYce3FrUYq78ZGQffBYCS1HmDPJdSTxUo").getKey();
    val key3 = new DumpedPrivateKey(params, "cVHwXSPRZmL9adctwBwmn4oTZdZMbaCsR5XF6VznqMgcvt1FDDxg").getKey();
    val multisigScript = ScriptBuilder.createMultiSigOutputScript(2, util.Arrays.asList(key1, key2, key3));
    val sighash = spendTx.hashForSignature(0, multisigScript, SigHash.ALL, false);
    ScalacoinUtil.encodeHex(sighash.getBytes)

  }

}
