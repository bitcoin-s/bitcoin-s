package org.scalacoin.crypto

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


}
