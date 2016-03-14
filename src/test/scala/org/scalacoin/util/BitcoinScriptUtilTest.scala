package org.scalacoin.util

import org.scalacoin.script.bitwise.OP_EQUALVERIFY
import org.scalacoin.script.constant.{ScriptConstantImpl, BytesToPushOntoStackImpl, ScriptToken}
import org.scalacoin.script.crypto.{OP_CHECKSIG, OP_HASH160}
import org.scalacoin.script.stack.OP_DUP
import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 3/2/16.
 */
class BitcoinScriptUtilTest extends FlatSpec with MustMatchers {

  //from b30d3148927f620f5b1228ba941c211fdabdae75d0ba0b688a58accbf018f3cc
  val asm = TestUtil.p2pkhScriptPubKey.asm
  val expectedHex = TestUtil.rawP2PKHScriptPubKey
  "BitcoinScriptUtil" must "give us the correct hexadecimal value of an asm script" in {

    BitcoinScriptUtil.asmToHex(asm) must be (expectedHex)
  }

  it must "give us the correct byte representation of an asm script" in {
    BitcoinScriptUtil.asmToBytes(asm) must be (BitcoinSUtil.decodeHex(expectedHex))
  }

}
