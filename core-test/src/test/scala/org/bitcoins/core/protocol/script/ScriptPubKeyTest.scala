package org.bitcoins.core.protocol.script

import org.bitcoins.core.script.bitwise.OP_EQUALVERIFY
import org.bitcoins.core.script.constant._
import org.bitcoins.core.script.crypto.{OP_CHECKSIG, OP_HASH160}
import org.bitcoins.core.script.stack.OP_DUP
import org.bitcoins.crypto.{CryptoUtil, ECPublicKey}
import org.bitcoins.testkit.Implicits._
import org.bitcoins.testkit.core.gen.CryptoGenerators
import org.bitcoins.testkit.util.{BitcoinSUnitTest, TestUtil}

/**
  * Created by chris on 1/14/16.
  */
class ScriptPubKeyTest extends BitcoinSUnitTest {

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

  "ScriptPubKey" must "give the expected asm from creating a scriptPubKey from hex" in {
    scriptPubKey.asm must be(expectedAsm)
  }

  it must "determine if we have a witness program inside of the scriptPubKey" in {
    val pubKeyHash =
      CryptoUtil.sha256Hash160(CryptoGenerators.publicKey.sampleSome.bytes)
    val witnessProgram = Seq(ScriptConstant(pubKeyHash.bytes))
    val asm = OP_0 +: BytesToPushOntoStack(20) +: witnessProgram
    val witnessScriptPubKey = WitnessScriptPubKey(asm)
    witnessScriptPubKey.witnessVersion must be(WitnessVersion0)
    witnessScriptPubKey.witnessProgram must be(witnessProgram)
  }

  it must "determine the correct descriptors" in {
    val key = ECPublicKey(
      "02c48670493ca813cd2d1bf8177df3d3d7c8e97fc7eb74cd21f71ea2ba416aee54")
    // p2pk
    val p2pk = P2PKScriptPubKey(key)
    assert(p2pk.toString == s"pk(${key.hex})")

    // p2pkh
    val p2pkh = P2PKHScriptPubKey(key)
    assert(p2pkh.toString == "pkh(63fe7c47cf475802b1c4ec2d34d1ef33e6b0fc63)")

    // multi
    val multi = MultiSignatureScriptPubKey(2, Seq(key, key))
    assert(
      multi.toString == "multi(2,02c48670493ca813cd2d1bf8177df3d3d7c8e97fc7eb74cd21f71ea2ba416aee54,02c48670493ca813cd2d1bf8177df3d3d7c8e97fc7eb74cd21f71ea2ba416aee54)")

    // p2sh
    val p2sh = P2SHScriptPubKey(p2pkh)
    assert(p2sh.toString == "sh(2a941c7a3e92c7f5fe149a641cae6b417989c411)")

    //p2wpkh
    val p2wpkh = P2WPKHWitnessSPKV0(key)
    assert(p2wpkh.toString == "wpkh(63fe7c47cf475802b1c4ec2d34d1ef33e6b0fc63)")

    // p2wsh
    val wsh = P2WSHWitnessSPKV0(p2pkh)
    assert(
      wsh.toString == "wsh(c0ad050ea2824ca0b938dd1c998f7160793034f321a307aae990786c0c029317)")
  }
}
