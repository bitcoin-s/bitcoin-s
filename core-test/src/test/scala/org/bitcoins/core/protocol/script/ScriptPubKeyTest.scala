package org.bitcoins.core.protocol.script

import org.bitcoins.core.crypto.ECPrivateKey
import org.bitcoins.core.gen.CryptoGenerators
import org.bitcoins.core.script.bitwise.OP_EQUALVERIFY
import org.bitcoins.core.script.constant._
import org.bitcoins.core.script.crypto.{ OP_CHECKSIG, OP_CODESEPARATOR, OP_HASH160 }
import org.bitcoins.core.script.locktime.{ OP_CHECKLOCKTIMEVERIFY, OP_CHECKSEQUENCEVERIFY }
import org.bitcoins.core.script.stack.{ OP_DROP, OP_DUP }
import org.bitcoins.core.util.{ CryptoUtil, TestUtil }
import org.scalatest.{ FlatSpec, MustMatchers }

/**
 * Created by chris on 1/14/16.
 */
class ScriptPubKeyTest extends FlatSpec with MustMatchers {
  val expectedAsm: Seq[ScriptToken] =
    List(OP_DUP, OP_HASH160, BytesToPushOntoStack(20), ScriptConstant("31a420903c05a0a7de2de40c9f02ebedbacdc172"), OP_EQUALVERIFY, OP_CHECKSIG)
  //from b30d3148927f620f5b1228ba941c211fdabdae75d0ba0b688a58accbf018f3cc
  val rawScriptPubKey = TestUtil.rawP2PKHScriptPubKey
  val scriptPubKey = ScriptPubKey(rawScriptPubKey)

  "ScriptPubKey" must "give the expected asm from creating a scriptPubKey from hex" in {
    scriptPubKey.asm must be(expectedAsm)
  }

  it must "determine if we have a witness program inside of the scriptPubKey" in {
    val pubKeyHash = CryptoUtil.sha256Hash160(CryptoGenerators.publicKey.sample.get.bytes)
    val witnessProgram = Seq(ScriptConstant(pubKeyHash.bytes))
    val asm = OP_0 +: BytesToPushOntoStack(20) +: witnessProgram
    val witnessScriptPubKey = WitnessScriptPubKey(asm)
    witnessScriptPubKey.isDefined must be(true)
    witnessScriptPubKey.get.witnessVersion must be(WitnessVersion0)
    witnessScriptPubKey.get.witnessProgram must be(witnessProgram)
  }
}

