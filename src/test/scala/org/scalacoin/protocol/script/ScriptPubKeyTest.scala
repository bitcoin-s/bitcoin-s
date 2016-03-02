package org.scalacoin.protocol.script

import org.scalacoin.script.bitwise.OP_EQUALVERIFY
import org.scalacoin.script.constant.{ScriptConstantImpl, BytesToPushOntoStackImpl, ScriptToken}
import org.scalacoin.script.crypto.{OP_CHECKSIG, OP_HASH160, OP_CODESEPARATOR}
import org.scalacoin.script.stack.OP_DUP
import org.scalacoin.util.{ScalacoinUtil, TestUtil}
import org.scalatest.{MustMatchers, FlatSpec}

/**
 * Created by chris on 1/14/16.
 */
class ScriptPubKeyTest extends FlatSpec with MustMatchers {


  val expectedAsm : Seq[ScriptToken] =
    List(OP_DUP, OP_HASH160, BytesToPushOntoStackImpl(20), ScriptConstantImpl("31a420903c05a0a7de2de40c9f02ebedbacdc172"), OP_EQUALVERIFY, OP_CHECKSIG)
  //from b30d3148927f620f5b1228ba941c211fdabdae75d0ba0b688a58accbf018f3cc
  val rawScriptPubKey = "1976a91431a420903c05a0a7de2de40c9f02ebedbacdc17288ac"
  val scriptPubKey = ScriptPubKeyFactory.fromHex(rawScriptPubKey)
  "ScriptPubKey"  must "give the expected asm from creating a scriptPubKey from hex" in {
    scriptPubKey.asm must be (expectedAsm)
  }
}
