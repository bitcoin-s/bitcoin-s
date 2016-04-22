package org.scalacoin.util

import org.scalacoin.script.bitwise.OP_EQUALVERIFY
import org.scalacoin.script.constant._
import org.scalacoin.script.crypto._
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

  it must "count 0 sigops where there are none in a script" in {
    val script = Seq()
    BitcoinScriptUtil.countSigOps(script) must be (0)
    BitcoinScriptUtil.countSigOps(Seq(OP_1,OP_2,ScriptConstantFactory.fromHex("1234")))
  }


  it must "count n sigops when we have n occurrences OP_CHECKSIG or OP_CHECKSIGVERIFY in a script" in {
    BitcoinScriptUtil.countSigOps(Seq(OP_CHECKSIG)) must be (1)
    BitcoinScriptUtil.countSigOps(Seq(OP_CHECKSIGVERIFY)) must be (1)

    BitcoinScriptUtil.countSigOps(Seq(OP_CHECKSIG,OP_CHECKSIG,OP_CHECKSIG,OP_CHECKSIG,OP_CHECKSIG)) must be (5)
    BitcoinScriptUtil.countSigOps(Seq(OP_CHECKSIGVERIFY,OP_CHECKSIGVERIFY,
      OP_CHECKSIGVERIFY,OP_CHECKSIGVERIFY,OP_CHECKSIGVERIFY)) must be (5)
  }

  it must "count n sigops when have n possible signatures in a OP_CHECKMULTISIG or OP_CHECKMULTISIGVERIFY" in {
    BitcoinScriptUtil.countSigOps(Seq(OP_0, OP_CHECKMULTISIG)) must be (0)
    BitcoinScriptUtil.countSigOps(Seq(OP_0, OP_CHECKMULTISIGVERIFY)) must be (0)

    BitcoinScriptUtil.countSigOps(Seq(OP_1, OP_CHECKMULTISIG)) must be (1)
    BitcoinScriptUtil.countSigOps(Seq(OP_1, OP_CHECKMULTISIGVERIFY)) must be (1)

    BitcoinScriptUtil.countSigOps(Seq(OP_16, OP_CHECKMULTISIG)) must be (16)
    BitcoinScriptUtil.countSigOps(Seq(OP_16, OP_CHECKMULTISIGVERIFY)) must be (16)
  }

  it must "count n sigops when have n possible signatures with multiple occurrences OP_CHECKMULTISIG or OP_CHECKMULTISIGVERIFY in a script" in {
    BitcoinScriptUtil.countSigOps(Seq(OP_0, OP_CHECKMULTISIG,OP_0, OP_CHECKMULTISIG)) must be (0)
    BitcoinScriptUtil.countSigOps(Seq(OP_0, OP_CHECKMULTISIGVERIFY,OP_0, OP_CHECKMULTISIGVERIFY)) must be (0)

    BitcoinScriptUtil.countSigOps(Seq(OP_1, OP_CHECKMULTISIG,OP_1, OP_CHECKMULTISIG)) must be (2)
    BitcoinScriptUtil.countSigOps(Seq(OP_1, OP_CHECKMULTISIGVERIFY,OP_1, OP_CHECKMULTISIGVERIFY)) must be (2)

    BitcoinScriptUtil.countSigOps(Seq(OP_16, OP_CHECKMULTISIG,OP_16, OP_CHECKMULTISIG)) must be (32)
    BitcoinScriptUtil.countSigOps(Seq(OP_16, OP_CHECKMULTISIGVERIFY,OP_16, OP_CHECKMULTISIG)) must be (32)
  }


  it must "" in {

    val script = List(OP_NOP, OP_NOP, OP_NOP, OP_NOP, OP_NOP, OP_NOP, OP_NOP, OP_NOP,
      OP_NOP, OP_NOP, OP_NOP, OP_NOP, OP_NOP, OP_0, OP_0, BytesToPushOntoStackImpl(1),
      ScriptConstantImpl("61"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("62"),
      BytesToPushOntoStackImpl(1), ScriptConstantImpl("63"), BytesToPushOntoStackImpl(1),
      ScriptConstantImpl("64"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("65"),
      BytesToPushOntoStackImpl(1), ScriptConstantImpl("66"), BytesToPushOntoStackImpl(1),
      ScriptConstantImpl("67"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("68"),
      BytesToPushOntoStackImpl(1), ScriptConstantImpl("69"), BytesToPushOntoStackImpl(1),
      ScriptConstantImpl("6a"), BytesToPushOntoStackImpl(1),
      ScriptConstantImpl("6b"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("6c"),
      BytesToPushOntoStackImpl(1), ScriptConstantImpl("6d"), BytesToPushOntoStackImpl(1),
      ScriptConstantImpl("6e"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("6f"),
      BytesToPushOntoStackImpl(1), ScriptConstantImpl("70"), BytesToPushOntoStackImpl(1),
      ScriptConstantImpl("71"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("72"),
      BytesToPushOntoStackImpl(1), ScriptConstantImpl("73"), BytesToPushOntoStackImpl(1),
      ScriptConstantImpl("74"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("14"), OP_CHECKMULTISIG,
      OP_0, OP_0, BytesToPushOntoStackImpl(1), ScriptConstantImpl("61"), BytesToPushOntoStackImpl(1),
      ScriptConstantImpl("62"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("63"), BytesToPushOntoStackImpl(1),
      ScriptConstantImpl("64"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("65"), BytesToPushOntoStackImpl(1),
      ScriptConstantImpl("66"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("67"), BytesToPushOntoStackImpl(1),
      ScriptConstantImpl("68"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("69"), BytesToPushOntoStackImpl(1),
      ScriptConstantImpl("6a"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("6b"), BytesToPushOntoStackImpl(1),
      ScriptConstantImpl("6c"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("6d"), BytesToPushOntoStackImpl(1),
      ScriptConstantImpl("6e"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("6f"), BytesToPushOntoStackImpl(1),
      ScriptConstantImpl("70"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("71"), BytesToPushOntoStackImpl(1),
      ScriptConstantImpl("72"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("73"), BytesToPushOntoStackImpl(1),
      ScriptConstantImpl("74"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("14"), OP_CHECKMULTISIG,
      OP_0, OP_0, BytesToPushOntoStackImpl(1), ScriptConstantImpl("61"), BytesToPushOntoStackImpl(1),
      ScriptConstantImpl("62"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("63"), BytesToPushOntoStackImpl(1),
      ScriptConstantImpl("64"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("65"), BytesToPushOntoStackImpl(1),
      ScriptConstantImpl("66"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("67"), BytesToPushOntoStackImpl(1),
      ScriptConstantImpl("68"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("69"), BytesToPushOntoStackImpl(1),
      ScriptConstantImpl("6a"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("6b"), BytesToPushOntoStackImpl(1),
      ScriptConstantImpl("6c"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("6d"), BytesToPushOntoStackImpl(1),
      ScriptConstantImpl("6e"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("6f"), BytesToPushOntoStackImpl(1),
      ScriptConstantImpl("70"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("71"), BytesToPushOntoStackImpl(1),
      ScriptConstantImpl("72"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("73"), BytesToPushOntoStackImpl(1),
      ScriptConstantImpl("74"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("14"), OP_CHECKMULTISIG,
      OP_0, OP_0, BytesToPushOntoStackImpl(1), ScriptConstantImpl("61"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("62"),
      BytesToPushOntoStackImpl(1), ScriptConstantImpl("63"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("64"),
      BytesToPushOntoStackImpl(1), ScriptConstantImpl("65"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("66"),
      BytesToPushOntoStackImpl(1), ScriptConstantImpl("67"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("68"),
      BytesToPushOntoStackImpl(1), ScriptConstantImpl("69"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("6a"),
      BytesToPushOntoStackImpl(1), ScriptConstantImpl("6b"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("6c"),
      BytesToPushOntoStackImpl(1), ScriptConstantImpl("6d"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("6e"),
      BytesToPushOntoStackImpl(1), ScriptConstantImpl("6f"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("70"),
      BytesToPushOntoStackImpl(1), ScriptConstantImpl("71"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("72"),
      BytesToPushOntoStackImpl(1), ScriptConstantImpl("73"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("74"),
      BytesToPushOntoStackImpl(1), ScriptConstantImpl("14"), OP_CHECKMULTISIG, OP_0, OP_0, BytesToPushOntoStackImpl(1),
      ScriptConstantImpl("61"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("62"), BytesToPushOntoStackImpl(1),
      ScriptConstantImpl("63"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("64"), BytesToPushOntoStackImpl(1),
      ScriptConstantImpl("65"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("66"), BytesToPushOntoStackImpl(1),
      ScriptConstantImpl("67"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("68"), BytesToPushOntoStackImpl(1),
      ScriptConstantImpl("69"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("6a"), BytesToPushOntoStackImpl(1),
      ScriptConstantImpl("6b"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("6c"), BytesToPushOntoStackImpl(1),
      ScriptConstantImpl("6d"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("6e"), BytesToPushOntoStackImpl(1),
      ScriptConstantImpl("6f"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("70"), BytesToPushOntoStackImpl(1),
      ScriptConstantImpl("71"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("72"), BytesToPushOntoStackImpl(1),
      ScriptConstantImpl("73"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("74"), BytesToPushOntoStackImpl(1),
      ScriptConstantImpl("14"), OP_CHECKMULTISIG, OP_0, OP_0, BytesToPushOntoStackImpl(1), ScriptConstantImpl("61"),
      BytesToPushOntoStackImpl(1), ScriptConstantImpl("62"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("63"),
      BytesToPushOntoStackImpl(1), ScriptConstantImpl("64"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("65"),
      BytesToPushOntoStackImpl(1), ScriptConstantImpl("66"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("67"),
      BytesToPushOntoStackImpl(1), ScriptConstantImpl("68"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("69"),
      BytesToPushOntoStackImpl(1), ScriptConstantImpl("6a"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("6b"),
      BytesToPushOntoStackImpl(1), ScriptConstantImpl("6c"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("6d"),
      BytesToPushOntoStackImpl(1), ScriptConstantImpl("6e"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("6f"),
      BytesToPushOntoStackImpl(1), ScriptConstantImpl("70"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("71"),
      BytesToPushOntoStackImpl(1), ScriptConstantImpl("72"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("73"),
      BytesToPushOntoStackImpl(1), ScriptConstantImpl("74"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("14"),
      OP_CHECKMULTISIG, OP_0, OP_0, BytesToPushOntoStackImpl(1), ScriptConstantImpl("61"), BytesToPushOntoStackImpl(1),
      ScriptConstantImpl("62"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("63"), BytesToPushOntoStackImpl(1),
      ScriptConstantImpl("64"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("65"), BytesToPushOntoStackImpl(1),
      ScriptConstantImpl("66"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("67"), BytesToPushOntoStackImpl(1),
      ScriptConstantImpl("68"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("69"), BytesToPushOntoStackImpl(1),
      ScriptConstantImpl("6a"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("6b"), BytesToPushOntoStackImpl(1),
      ScriptConstantImpl("6c"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("6d"), BytesToPushOntoStackImpl(1),
      ScriptConstantImpl("6e"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("6f"), BytesToPushOntoStackImpl(1),
      ScriptConstantImpl("70"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("71"), BytesToPushOntoStackImpl(1),
      ScriptConstantImpl("72"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("73"), BytesToPushOntoStackImpl(1),
      ScriptConstantImpl("74"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("14"), OP_CHECKMULTISIG, OP_0, OP_0,
      BytesToPushOntoStackImpl(1), ScriptConstantImpl("61"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("62"),
      BytesToPushOntoStackImpl(1), ScriptConstantImpl("63"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("64"),
      BytesToPushOntoStackImpl(1), ScriptConstantImpl("65"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("66"),
      BytesToPushOntoStackImpl(1), ScriptConstantImpl("67"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("68"),
      BytesToPushOntoStackImpl(1), ScriptConstantImpl("69"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("6a"),
      BytesToPushOntoStackImpl(1), ScriptConstantImpl("6b"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("6c"),
      BytesToPushOntoStackImpl(1), ScriptConstantImpl("6d"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("6e"),
      BytesToPushOntoStackImpl(1), ScriptConstantImpl("6f"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("70"),
      BytesToPushOntoStackImpl(1), ScriptConstantImpl("71"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("72"),
      BytesToPushOntoStackImpl(1), ScriptConstantImpl("73"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("74"),
      BytesToPushOntoStackImpl(1), ScriptConstantImpl("14"), OP_CHECKMULTISIG, OP_0, OP_0, BytesToPushOntoStackImpl(1),
      ScriptConstantImpl("61"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("62"), BytesToPushOntoStackImpl(1),
      ScriptConstantImpl("63"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("64"), BytesToPushOntoStackImpl(1),
      ScriptConstantImpl("65"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("66"), BytesToPushOntoStackImpl(1),
      ScriptConstantImpl("67"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("68"), BytesToPushOntoStackImpl(1),
      ScriptConstantImpl("69"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("6a"), BytesToPushOntoStackImpl(1),
      ScriptConstantImpl("6b"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("6c"), BytesToPushOntoStackImpl(1),
      ScriptConstantImpl("6d"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("6e"), BytesToPushOntoStackImpl(1),
      ScriptConstantImpl("6f"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("70"), BytesToPushOntoStackImpl(1),
      ScriptConstantImpl("71"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("72"), BytesToPushOntoStackImpl(1),
      ScriptConstantImpl("73"), BytesToPushOntoStackImpl(1), ScriptConstantImpl("74"), BytesToPushOntoStackImpl(1),
      ScriptConstantImpl("14"), OP_CHECKMULTISIG)

    BitcoinScriptUtil.countSigOps(script) must be (180)
  }
}
