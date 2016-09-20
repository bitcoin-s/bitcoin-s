package org.bitcoins.core.protocol.script

import org.bitcoins.core.crypto.ECPrivateKey
import org.bitcoins.core.script.bitwise.OP_EQUALVERIFY
import org.bitcoins.core.script.constant.{ScriptNumber, BytesToPushOntoStack, ScriptConstant, ScriptToken}
import org.bitcoins.core.script.crypto.{OP_CHECKSIG, OP_CODESEPARATOR, OP_HASH160}
import org.bitcoins.core.script.locktime.{OP_CHECKSEQUENCEVERIFY, OP_CHECKLOCKTIMEVERIFY}
import org.bitcoins.core.script.stack.{OP_DROP, OP_DUP}
import org.bitcoins.core.util.{CryptoUtil, TestUtil}
import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 1/14/16.
 */
class ScriptPubKeyTest extends FlatSpec with MustMatchers {


  val expectedAsm : Seq[ScriptToken] =
    List(OP_DUP, OP_HASH160, BytesToPushOntoStack(20), ScriptConstant("31a420903c05a0a7de2de40c9f02ebedbacdc172"), OP_EQUALVERIFY, OP_CHECKSIG)
  //from b30d3148927f620f5b1228ba941c211fdabdae75d0ba0b688a58accbf018f3cc
  val rawScriptPubKey = TestUtil.rawP2PKHScriptPubKey
  val scriptPubKey = ScriptPubKey(rawScriptPubKey)

  "ScriptPubKey"  must "give the expected asm from creating a scriptPubKey from hex" in {
    scriptPubKey.asm must be (expectedAsm)
  }

  "CLTVScriptPubKey" must "return the expected asm from hex" in {
    val expectedCLTVAsm : Seq[ScriptToken] =
      List(BytesToPushOntoStack(4), ScriptConstant("2b07ae57"), OP_CHECKLOCKTIMEVERIFY, OP_DROP) ++ expectedAsm
    val cltv = CLTVScriptPubKey(ScriptNumber(1471022891), scriptPubKey)
    cltv.asm must be (expectedCLTVAsm)
  }

  val scriptNum17 = ScriptNumber(17)
  val scriptNum5 = ScriptNumber(5)
  val negativeOne = ScriptNumber(-1)
  val pubKey = ECPrivateKey().publicKey
  val p2pkh = P2PKHScriptPubKey(pubKey)

  it must "determine the correct underlying scriptPubKey inside a CLTVScriptPubKey" in {
    CLTVScriptPubKey(scriptNum17, p2pkh).scriptPubKeyAfterCLTV must be (p2pkh)
    CLTVScriptPubKey(scriptNum5, p2pkh).scriptPubKeyAfterCLTV must be (p2pkh)
    CLTVScriptPubKey(negativeOne, p2pkh).scriptPubKeyAfterCLTV must be (p2pkh)
  }

  it must "determine the correct locktime for the CLTVScriptPubKey" in {
    CLTVScriptPubKey(scriptNum17, p2pkh).locktime must be (scriptNum17)
    CLTVScriptPubKey(scriptNum5, p2pkh).locktime must be (scriptNum5)
  }

  "CSVScriptPubKey" must "return the expected asm from hex" in {
    val expectedCSVAsm : Seq[ScriptToken] =
      List(BytesToPushOntoStack(4), ScriptConstant("6202b257"), OP_CHECKSEQUENCEVERIFY, OP_DROP) ++ expectedAsm
    val csv = CSVScriptPubKey(ScriptNumber(1471283810), scriptPubKey)
    csv.asm must be (expectedCSVAsm)
  }

  it must "determine the correct underlying scriptPubKey inside a CLTVScriptPubKey" in {
    CSVScriptPubKey(scriptNum17, p2pkh).scriptPubKeyAfterCSV must be (p2pkh)
    CSVScriptPubKey(scriptNum5, p2pkh).scriptPubKeyAfterCSV must be (p2pkh)
    CSVScriptPubKey(negativeOne, p2pkh).scriptPubKeyAfterCSV must be (p2pkh)
  }

  it must "determine the correct locktime for the CLTVScriptPubKey" in {
    CSVScriptPubKey(scriptNum17, p2pkh).locktime must be (scriptNum17)
    CSVScriptPubKey(scriptNum5, p2pkh).locktime must be (scriptNum5)
  }
}
