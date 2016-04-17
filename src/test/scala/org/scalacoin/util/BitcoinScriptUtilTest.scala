package org.scalacoin.util

import org.scalacoin.script.bitwise.OP_EQUALVERIFY
import org.scalacoin.script.constant._
import org.scalacoin.script.crypto.{OP_CHECKSIG, OP_HASH160}
import org.scalacoin.script.locktime.OP_CHECKLOCKTIMEVERIFY
import org.scalacoin.script.reserved.{OP_NOP, OP_RESERVED}
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

  it must "filter out all of the push operations in a scriptSig" in {
    BitcoinScriptUtil.filterPushOps(Seq(OP_PUSHDATA1, OP_PUSHDATA2, OP_PUSHDATA4) ++
      BytesToPushOntoStackFactory.operations).isEmpty must be (true)
  }

  it must "determine if a script op count towards the bitcoin script op code limit" in {
    BitcoinScriptUtil.countsTowardsScriptOpLimit(OP_1) must be (false)
    BitcoinScriptUtil.countsTowardsScriptOpLimit(OP_16) must be (false)
    BitcoinScriptUtil.countsTowardsScriptOpLimit(OP_RESERVED) must be (false)

    BitcoinScriptUtil.countsTowardsScriptOpLimit(OP_NOP) must be (true)
    BitcoinScriptUtil.countsTowardsScriptOpLimit(OP_CHECKLOCKTIMEVERIFY) must be (true)
  }

  it must "not count script constants towards the script count limit" in {
    BitcoinScriptUtil.countsTowardsScriptOpLimit(ScriptConstantFactory.fromHex("1234")) must be (false)
  }

  it must "not count OP_PUSHDATA operations towards the script count" in {
    BitcoinScriptUtil.countsTowardsScriptOpLimit(OP_PUSHDATA1) must be (false)
    BitcoinScriptUtil.countsTowardsScriptOpLimit(OP_PUSHDATA2) must be (false)
    BitcoinScriptUtil.countsTowardsScriptOpLimit(OP_PUSHDATA4) must be (false)
  }

}
