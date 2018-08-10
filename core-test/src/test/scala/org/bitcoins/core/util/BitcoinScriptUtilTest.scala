package org.bitcoins.core.util

import org.bitcoins.core.crypto.{ ECPrivateKey, ECPublicKey }
import org.bitcoins.core.protocol.script.{ SigVersionBase, SigVersionWitnessV0 }
import org.bitcoins.core.script.constant._
import org.bitcoins.core.script.crypto._
import org.bitcoins.core.script.flag.ScriptVerifyWitnessPubKeyType
import org.bitcoins.core.script.locktime.OP_CHECKLOCKTIMEVERIFY
import org.bitcoins.core.script.reserved.{ OP_NOP, OP_RESERVED }
import org.bitcoins.core.script.result.ScriptErrorWitnessPubKeyType
import org.scalatest.{ FlatSpec, MustMatchers }
import scodec.bits.ByteVector

/**
 * Created by chris on 3/2/16.
 */
class BitcoinScriptUtilTest extends FlatSpec with MustMatchers {

  //from b30d3148927f620f5b1228ba941c211fdabdae75d0ba0b688a58accbf018f3cc
  val asm = TestUtil.p2pkhScriptPubKey.asm
  val expectedHex = TestUtil.rawP2PKHScriptPubKey
  "BitcoinScriptUtil" must "give us the correct hexadecimal value of an asm script" in {

    BitcoinScriptUtil.asmToHex(asm) must be(asm.flatMap(_.hex).mkString)
  }

  it must "filter out all of the push operations in a scriptSig" in {
    BitcoinScriptUtil.filterPushOps(Seq(OP_PUSHDATA1, OP_PUSHDATA2, OP_PUSHDATA4) ++
      BytesToPushOntoStack.operations).isEmpty must be(true)
  }

  it must "determine if a script op count towards the bitcoin script op code limit" in {
    BitcoinScriptUtil.countsTowardsScriptOpLimit(OP_1) must be(false)
    BitcoinScriptUtil.countsTowardsScriptOpLimit(OP_16) must be(false)
    BitcoinScriptUtil.countsTowardsScriptOpLimit(OP_RESERVED) must be(false)

    BitcoinScriptUtil.countsTowardsScriptOpLimit(OP_NOP) must be(true)
    BitcoinScriptUtil.countsTowardsScriptOpLimit(OP_CHECKLOCKTIMEVERIFY) must be(true)
  }

  it must "not count script constants towards the script count limit" in {
    BitcoinScriptUtil.countsTowardsScriptOpLimit(ScriptConstant("1234")) must be(false)
  }

  it must "not count OP_PUSHDATA operations towards the script count" in {
    BitcoinScriptUtil.countsTowardsScriptOpLimit(OP_PUSHDATA1) must be(false)
    BitcoinScriptUtil.countsTowardsScriptOpLimit(OP_PUSHDATA2) must be(false)
    BitcoinScriptUtil.countsTowardsScriptOpLimit(OP_PUSHDATA4) must be(false)
  }

  it must "count 0 sigops where there are none in a script" in {
    val script = Seq()
    BitcoinScriptUtil.countSigOps(script) must be(0)
    BitcoinScriptUtil.countSigOps(Seq(OP_1, OP_2, ScriptConstant("1234")))
  }

  it must "count n sigops when we have n occurrences OP_CHECKSIG or OP_CHECKSIGVERIFY in a script" in {
    BitcoinScriptUtil.countSigOps(Seq(OP_CHECKSIG)) must be(1)
    BitcoinScriptUtil.countSigOps(Seq(OP_CHECKSIGVERIFY)) must be(1)

    BitcoinScriptUtil.countSigOps(Seq(OP_CHECKSIG, OP_CHECKSIG, OP_CHECKSIG, OP_CHECKSIG, OP_CHECKSIG)) must be(5)
    BitcoinScriptUtil.countSigOps(Seq(OP_CHECKSIGVERIFY, OP_CHECKSIGVERIFY,
      OP_CHECKSIGVERIFY, OP_CHECKSIGVERIFY, OP_CHECKSIGVERIFY)) must be(5)
  }

  it must "count n sigops when have n possible signatures in a OP_CHECKMULTISIG or OP_CHECKMULTISIGVERIFY" in {
    BitcoinScriptUtil.countSigOps(Seq(OP_0, OP_CHECKMULTISIG)) must be(0)
    BitcoinScriptUtil.countSigOps(Seq(OP_0, OP_CHECKMULTISIGVERIFY)) must be(0)

    BitcoinScriptUtil.countSigOps(Seq(OP_1, OP_CHECKMULTISIG)) must be(1)
    BitcoinScriptUtil.countSigOps(Seq(OP_1, OP_CHECKMULTISIGVERIFY)) must be(1)

    BitcoinScriptUtil.countSigOps(Seq(OP_16, OP_CHECKMULTISIG)) must be(16)
    BitcoinScriptUtil.countSigOps(Seq(OP_16, OP_CHECKMULTISIGVERIFY)) must be(16)
  }

  it must "count n sigops when have n possible signatures with multiple occurrences OP_CHECKMULTISIG or OP_CHECKMULTISIGVERIFY in a script" in {
    BitcoinScriptUtil.countSigOps(Seq(OP_0, OP_CHECKMULTISIG, OP_0, OP_CHECKMULTISIG)) must be(0)
    BitcoinScriptUtil.countSigOps(Seq(OP_0, OP_CHECKMULTISIGVERIFY, OP_0, OP_CHECKMULTISIGVERIFY)) must be(0)

    BitcoinScriptUtil.countSigOps(Seq(OP_1, OP_CHECKMULTISIG, OP_1, OP_CHECKMULTISIG)) must be(2)
    BitcoinScriptUtil.countSigOps(Seq(OP_1, OP_CHECKMULTISIGVERIFY, OP_1, OP_CHECKMULTISIGVERIFY)) must be(2)

    BitcoinScriptUtil.countSigOps(Seq(OP_16, OP_CHECKMULTISIG, OP_16, OP_CHECKMULTISIG)) must be(32)
    BitcoinScriptUtil.countSigOps(Seq(OP_16, OP_CHECKMULTISIGVERIFY, OP_16, OP_CHECKMULTISIG)) must be(32)
  }

  it must "determine if the empty script is push only" in {
    BitcoinScriptUtil.isPushOnly(Seq()) must be(true)
    BitcoinScriptUtil.isPushOnly(Seq(OP_0)) must be(true)
  }

  it must "determine that script is not push only if it contains any operation whose op code is > OP_16.opCode" in {
    BitcoinScriptUtil.isPushOnly(Seq(OP_NOP, OP_0, OP_0, OP_0)) must be(false)
  }

  it must "determine that a script is not push only if it contains a ScriptConstant" in {
    BitcoinScriptUtil.isPushOnly(Seq(OP_NOP, BytesToPushOntoStack(1), ScriptConstant("51"))) must be(false)
  }

  it must "determine that the script is not push only if it contains an script number operation" in {
    BitcoinScriptUtil.isMinimalPush(BytesToPushOntoStack(1), OP_1) must be(false)
  }

  it must "determine that a script is push only if it only contains pushing an empty script constant" in {
    BitcoinScriptUtil.isMinimalPush(OP_0, ScriptConstant("")) must be(true)
  }

  it must "determine that a script is not push only if it uses an OP_PUSHDATA operation while pushing < 75 bytes" in {
    BitcoinScriptUtil.isMinimalPush(OP_PUSHDATA1, OP_0) must be(false)
  }

  it must "determine that a OP_PUSHDATA1 operation is the minimal push op for a 76 byte script constant" in {
    val byteConstantSize = 76
    val byteConstant = Array.fill(byteConstantSize)(0.toByte)
    val scriptConstant = ScriptConstant(ByteVector(byteConstant))
    BitcoinScriptUtil.isMinimalPush(OP_PUSHDATA1, scriptConstant) must be(true)
  }

  it must "determine that a OP_PUSHDATA1 operation is NOT the minimal push op for a 75 byte script constant" in {
    val byteConstantSize = 75
    val byteConstant = Array.fill(byteConstantSize)(0.toByte)
    val scriptConstant = ScriptConstant(ByteVector(byteConstant))
    BitcoinScriptUtil.isMinimalPush(OP_PUSHDATA1, scriptConstant) must be(false)
  }

  it must "determine that a OP_PUSHDATA2 operation is NOT the minimal push op for a 255 byte script constant" in {
    val byteConstantSize = 255
    val byteConstant = Array.fill(byteConstantSize)(0.toByte)
    val scriptConstant = ScriptConstant(ByteVector(byteConstant))
    BitcoinScriptUtil.isMinimalPush(OP_PUSHDATA2, scriptConstant) must be(false)
  }

  it must "determine that a OP_PUSHDATA2 operation is the minimal push op for a 256 byte script constant" in {
    val byteConstantSize = 256
    val byteConstant = Array.fill(byteConstantSize)(0.toByte)
    val scriptConstant = ScriptConstant(ByteVector(byteConstant))
    BitcoinScriptUtil.isMinimalPush(OP_PUSHDATA2, scriptConstant) must be(true)
  }

  it must "determine that a OP_PUSHDATA4 operation is NOT the minimal push op for a 65535 byte script constant" in {
    val byteConstantSize = 65535
    val byteConstant = Array.fill(byteConstantSize)(0.toByte)
    val scriptConstant = ScriptConstant(ByteVector(byteConstant))
    BitcoinScriptUtil.isMinimalPush(OP_PUSHDATA4, scriptConstant) must be(false)
  }

  it must "determine that a OP_PUSHDATA4 operation is the minimal push op for a 65536 byte script constant" in {
    val byteConstantSize = 65536
    val byteConstant = Array.fill(byteConstantSize)(0.toByte)
    val scriptConstant = ScriptConstant(ByteVector(byteConstant))
    BitcoinScriptUtil.isMinimalPush(OP_PUSHDATA4, scriptConstant) must be(true)
  }

  it must "determine if a number is encoded in the shortest way possible" in {
    BitcoinScriptUtil.isShortestEncoding(ScriptConstant("00")) must be(false)
    BitcoinScriptUtil.isShortestEncoding(ScriptConstant("0000")) must be(false)

    BitcoinScriptUtil.isShortestEncoding(ScriptConstant("0100")) must be(false)
    BitcoinScriptUtil.isShortestEncoding(ScriptConstant("80")) must be(false)
    BitcoinScriptUtil.isShortestEncoding(ScriptConstant("0080")) must be(false)

    BitcoinScriptUtil.isShortestEncoding(ScriptConstant("0500")) must be(false)
    BitcoinScriptUtil.isShortestEncoding(ScriptConstant("050000")) must be(false)

    BitcoinScriptUtil.isShortestEncoding(ScriptConstant("0580")) must be(false)
    BitcoinScriptUtil.isShortestEncoding(ScriptConstant("050080")) must be(false)

    BitcoinScriptUtil.isShortestEncoding(ScriptConstant("ff7f80")) must be(false)
    BitcoinScriptUtil.isShortestEncoding(ScriptConstant("ff7f00")) must be(false)

    BitcoinScriptUtil.isShortestEncoding(ScriptConstant("ffff7f80")) must be(false)
    BitcoinScriptUtil.isShortestEncoding(ScriptConstant("ffff7f00")) must be(false)
  }

  it must "check a public key's encoding" in {
    //pubkeys must be compressed or uncompressed or else that are not validly encoded
    val key = ECPublicKey("00")
    val program = TestUtil.testProgram
    BitcoinScriptUtil.checkPubKeyEncoding(key, program) must be(false)
  }

  it must "determine if script number is correctly minimally-encoded" in {
    val scriptNum100 = ScriptNumber(100)
    val scriptNum10 = ScriptNumber(10)
    val scriptNumZero = ScriptNumber(0)
    val scriptNum16 = ScriptNumber(16)
    val scriptNum17 = ScriptNumber(17)
    BitcoinScriptUtil.minimalScriptNumberRepresentation(scriptNum100) must be(scriptNum100)
    BitcoinScriptUtil.minimalScriptNumberRepresentation(scriptNum10) must be(OP_10)
    BitcoinScriptUtil.minimalScriptNumberRepresentation(scriptNumZero) must be(OP_0)
    BitcoinScriptUtil.minimalScriptNumberRepresentation(scriptNum16) must be(OP_16)
    BitcoinScriptUtil.minimalScriptNumberRepresentation(scriptNum17) must be(scriptNum17)
    BitcoinScriptUtil.minimalScriptNumberRepresentation(ScriptNumber(-1)) must be(OP_1NEGATE)
    BitcoinScriptUtil.minimalScriptNumberRepresentation(ScriptNumber(-2)) must be(ScriptNumber(-2))
  }

  it must "determine if a segwit pubkey is compressed" in {
    val key = ECPrivateKey(false)
    val pubKey = key.publicKey
    val flags = Seq(ScriptVerifyWitnessPubKeyType)
    BitcoinScriptUtil.isValidPubKeyEncoding(pubKey, SigVersionWitnessV0, flags) must be(Some(ScriptErrorWitnessPubKeyType))

    val key2 = ECPrivateKey(false)
    val pubKey2 = key2.publicKey
    BitcoinScriptUtil.isValidPubKeyEncoding(pubKey2, SigVersionBase, flags) must be(None)
  }

  it must "remove the signatures from a p2sh scriptSig" in {
    val p2shScriptSig = TestUtil.p2sh2Of3ScriptSig
    val signatures = p2shScriptSig.signatures
    val asmWithoutSigs = BitcoinScriptUtil.removeSignaturesFromScript(signatures, p2shScriptSig.asm)
    val sigExists = signatures.map(sig => asmWithoutSigs.exists(_ == ScriptConstant(sig.hex)))
    sigExists.exists(_ == true) must be(false)
  }

  it must "cast a script token to a boolean value" in {
    BitcoinScriptUtil.castToBool(ScriptConstant("")) must be(false)
    BitcoinScriptUtil.castToBool(ScriptConstant(scodec.bits.ByteVector(0x80.toByte))) must be(false)
    BitcoinScriptUtil.castToBool(ScriptConstant("000000")) must be(false)
    BitcoinScriptUtil.castToBool(ScriptConstant("00000080")) must be(false)

    BitcoinScriptUtil.castToBool(ScriptConstant("01")) must be(true)
    BitcoinScriptUtil.castToBool(ScriptConstant("80000000")) must be(true)
    BitcoinScriptUtil.castToBool(ScriptConstant("00008000")) must be(true)
  }
}
