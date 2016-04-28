package org.bitcoins.util

import org.bitcoins.crypto.ECFactory
import org.bitcoins.script.bitwise.OP_EQUALVERIFY
import org.bitcoins.script.constant._
import org.bitcoins.script.crypto._
import org.bitcoins.script.locktime.OP_CHECKLOCKTIMEVERIFY
import org.bitcoins.script.reserved.{OP_NOP, OP_RESERVED}
import org.bitcoins.script.stack.OP_DUP
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
      BytesToPushOntoStack.operations).isEmpty must be (true)
  }

  it must "determine if a script op count towards the bitcoin script op code limit" in {
    BitcoinScriptUtil.countsTowardsScriptOpLimit(OP_1) must be (false)
    BitcoinScriptUtil.countsTowardsScriptOpLimit(OP_16) must be (false)
    BitcoinScriptUtil.countsTowardsScriptOpLimit(OP_RESERVED) must be (false)

    BitcoinScriptUtil.countsTowardsScriptOpLimit(OP_NOP) must be (true)
    BitcoinScriptUtil.countsTowardsScriptOpLimit(OP_CHECKLOCKTIMEVERIFY) must be (true)
  }

  it must "not count script constants towards the script count limit" in {
    BitcoinScriptUtil.countsTowardsScriptOpLimit(ScriptConstant("1234")) must be (false)
  }

  it must "not count OP_PUSHDATA operations towards the script count" in {
    BitcoinScriptUtil.countsTowardsScriptOpLimit(OP_PUSHDATA1) must be (false)
    BitcoinScriptUtil.countsTowardsScriptOpLimit(OP_PUSHDATA2) must be (false)
    BitcoinScriptUtil.countsTowardsScriptOpLimit(OP_PUSHDATA4) must be (false)
  }

  it must "count 0 sigops where there are none in a script" in {
    val script = Seq()
    BitcoinScriptUtil.countSigOps(script) must be (0)
    BitcoinScriptUtil.countSigOps(Seq(OP_1,OP_2,ScriptConstant("1234")))
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

  it must "determine if the empty script is push only" in {
    BitcoinScriptUtil.isPushOnly(Seq()) must be (true)
    BitcoinScriptUtil.isPushOnly(Seq(OP_0)) must be (true)
  }

  it must "determine that script is not push only if it contains any operation whose op code is > OP_16.opCode" in {
    BitcoinScriptUtil.isPushOnly(Seq(OP_NOP, OP_0,OP_0,OP_0)) must be (false)
  }

  it must "determine that the script is not push only if it contains an script number operation" in {
    BitcoinScriptUtil.isMinimalPush(BytesToPushOntoStack(1).get,OP_1) must be (false)
  }

  it must "determine that a script is push only if it only contains pushing an empty script constant" in {
    BitcoinScriptUtil.isMinimalPush(OP_0,ScriptConstant("")) must be (true)
  }

  it must "determine that a script is not push only if it uses an OP_PUSHDATA operation while pushing < 75 bytes" in {
    BitcoinScriptUtil.isMinimalPush(OP_PUSHDATA1,OP_0) must be (false)
  }

  it must "determine that a OP_PUSHDATA1 operation is the minimal push op for a 76 byte script constant" in {
    val byteConstantSize = 76
    val byteConstant = for { x <- 0 to byteConstantSize} yield 0x0.toByte
    val scriptConstant = ScriptConstant(byteConstant)
    BitcoinScriptUtil.isMinimalPush(OP_PUSHDATA1, scriptConstant) must be (true)
  }

  it must "determine that a OP_PUSHDATA1 operation is NOT the minimal push op for a 75 byte script constant" in {
    val byteConstantSize = 75
    val byteConstant = for { x <- 0 until byteConstantSize } yield 0x0.toByte
    val scriptConstant = ScriptConstant(byteConstant)
    BitcoinScriptUtil.isMinimalPush(OP_PUSHDATA1, scriptConstant) must be (false)
  }

  it must "determine that a OP_PUSHDATA2 operation is NOT the minimal push op for a 255 byte script constant" in {
    val byteConstantSize = 255
    val byteConstant = for { x <- 0 until byteConstantSize } yield 0x0.toByte
    val scriptConstant = ScriptConstant(byteConstant)
    BitcoinScriptUtil.isMinimalPush(OP_PUSHDATA2, scriptConstant) must be (false)
  }

  it must "determine that a OP_PUSHDATA2 operation is the minimal push op for a 256 byte script constant" in {
    val byteConstantSize = 256
    val byteConstant = for { x <- 0 until byteConstantSize } yield 0x0.toByte
    val scriptConstant = ScriptConstant(byteConstant)
    BitcoinScriptUtil.isMinimalPush(OP_PUSHDATA2, scriptConstant) must be (true)
  }

  it must "determine that a OP_PUSHDATA4 operation is NOT the minimal push op for a 65535 byte script constant" in {
    val byteConstantSize = 65535
    val byteConstant = for { x <- 0 until byteConstantSize } yield 0x0.toByte
    val scriptConstant = ScriptConstant(byteConstant)
    BitcoinScriptUtil.isMinimalPush(OP_PUSHDATA4, scriptConstant) must be (false)
  }

  it must "determine that a OP_PUSHDATA4 operation is the minimal push op for a 65536 byte script constant" in {
    val byteConstantSize = 65536
    val byteConstant = for { x <- 0 until byteConstantSize } yield 0x0.toByte
    val scriptConstant = ScriptConstant(byteConstant)
    BitcoinScriptUtil.isMinimalPush(OP_PUSHDATA4, scriptConstant) must be (true)
  }


  it must "determine if a number is encoded in the shortest way possible" in {
    BitcoinScriptUtil.isShortestEncoding(ScriptConstant("00")) must be (false)
    BitcoinScriptUtil.isShortestEncoding(ScriptConstant("0000")) must be (false)

    BitcoinScriptUtil.isShortestEncoding(ScriptConstant("80")) must be (false)
    BitcoinScriptUtil.isShortestEncoding(ScriptConstant("0080")) must be (false)

    BitcoinScriptUtil.isShortestEncoding(ScriptConstant("0500")) must be (false)
    BitcoinScriptUtil.isShortestEncoding(ScriptConstant("050000")) must be (false)

    BitcoinScriptUtil.isShortestEncoding(ScriptConstant("0580")) must be (false)
    BitcoinScriptUtil.isShortestEncoding(ScriptConstant("050080")) must be (false)

    BitcoinScriptUtil.isShortestEncoding(ScriptConstant("ff7f80")) must be (false)
    BitcoinScriptUtil.isShortestEncoding(ScriptConstant("ff7f00")) must be (false)

    BitcoinScriptUtil.isShortestEncoding(ScriptConstant("ffff7f80")) must be (false)
    BitcoinScriptUtil.isShortestEncoding(ScriptConstant("ffff7f00")) must be (false)
  }

  it must "check a public key's encoding" in {
    //pubkeys must be compressed or uncompressed or else that are not validly encoded
    val key = ECFactory.publicKey("00")
    val program = TestUtil.testProgram
    BitcoinScriptUtil.checkPubKeyEncoding(key,program) must be (false)
  }
}
