package org.bitcoins.core.protocol.script

import org.bitcoins.core.script.bitwise.OP_EQUALVERIFY
import org.bitcoins.core.script.constant.{
  BytesToPushOntoStack,
  ScriptConstant,
  ScriptNumber,
  ScriptToken
}
import org.bitcoins.core.script.crypto.{OP_CHECKSIG, OP_HASH160}
import org.bitcoins.core.script.locktime.OP_CHECKLOCKTIMEVERIFY
import org.bitcoins.core.script.stack.{OP_DROP, OP_DUP}
import org.bitcoins.crypto.ECPrivateKey
import org.bitcoins.testkit.util.{BitcoinSUnitTest, TestUtil}

/**
  * Created by tom on 9/21/16.
  */
class CLTVScriptPubKeyTest extends BitcoinSUnitTest {

  val expectedAsm: Seq[ScriptToken] =
    List(OP_DUP,
         OP_HASH160,
         BytesToPushOntoStack(20),
         ScriptConstant("31a420903c05a0a7de2de40c9f02ebedbacdc172"),
         OP_EQUALVERIFY,
         OP_CHECKSIG)
  //from b30d3148927f620f5b1228ba941c211fdabdae75d0ba0b688a58accbf018f3cc
  val rawScriptPubKey = TestUtil.rawP2PKHScriptPubKey
  val scriptPubKey = ScriptPubKey(rawScriptPubKey)

  "CLTVScriptPubKey" must "return the expected asm from hex" in {
    val expectedCLTVAsm: Seq[ScriptToken] =
      List(BytesToPushOntoStack(4),
           ScriptConstant("2b07ae57"),
           OP_CHECKLOCKTIMEVERIFY,
           OP_DROP) ++ expectedAsm
    val cltv = CLTVScriptPubKey(ScriptNumber(1471022891), scriptPubKey)
    cltv.asm must be(expectedCLTVAsm)
  }

  it must "determine the correct underlying scriptPubKey, and locktime inside a CLTVScriptPubKey" in {
    val scriptNum17 = ScriptNumber(17)
    val scriptNum5 = ScriptNumber(5)
    val negativeOne = ScriptNumber(-1)
    val pubKey = ECPrivateKey().publicKey
    val p2pkh = P2PKHScriptPubKey(pubKey)

    CLTVScriptPubKey(scriptNum17, p2pkh).nestedScriptPubKey must be(p2pkh)
    CLTVScriptPubKey(scriptNum5, p2pkh).nestedScriptPubKey must be(p2pkh)
    CLTVScriptPubKey(negativeOne, p2pkh).nestedScriptPubKey must be(p2pkh)

    CLTVScriptPubKey(scriptNum17, p2pkh).locktime must be(scriptNum17)
    CLTVScriptPubKey(scriptNum5, p2pkh).locktime must be(scriptNum5)
  }
}
