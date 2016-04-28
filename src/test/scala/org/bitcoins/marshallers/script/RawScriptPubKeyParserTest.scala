package org.bitcoins.marshallers.script

import org.bitcoins.protocol.script.ScriptPubKey
import org.bitcoins.script.bitwise.OP_EQUALVERIFY
import org.bitcoins.script.constant.{ScriptToken, BytesToPushOntoStackImpl, ScriptConstantImpl}
import org.bitcoins.script.crypto.{OP_CHECKSIG, OP_HASH160}
import org.bitcoins.script.stack.OP_DUP
import org.bitcoins.util.TestUtil
import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 1/12/16.
 */
class RawScriptPubKeyParserTest extends FlatSpec with MustMatchers with RawScriptPubKeyParser  {


  "RawScriptPubKeyParser" must "parse a hex string into a scriptPubKey" in {
    val scriptPubKey : ScriptPubKey = read(TestUtil.rawScriptPubKey)
    scriptPubKey.asm must be (Seq(OP_DUP,OP_HASH160, BytesToPushOntoStackImpl(20),
      ScriptConstantImpl("cbc20a7664f2f69e5355aa427045bc15e7c6c772"),OP_EQUALVERIFY,OP_CHECKSIG))

  }

  it must "read then write the scriptPubKey and get the original scriptPubKey" in {
    val scriptPubKey : ScriptPubKey = read(TestUtil.rawScriptPubKey)
    write(scriptPubKey) must be (TestUtil.rawScriptPubKey)
  }

  it must "read a raw scriptPubKey and give us the expected asm" in {
    val scriptPubKey = read(TestUtil.rawP2PKHScriptPubKey)
    val expectedAsm : Seq[ScriptToken] =
      List(OP_DUP, OP_HASH160, BytesToPushOntoStackImpl(20), ScriptConstantImpl("31a420903c05a0a7de2de40c9f02ebedbacdc172"),
        OP_EQUALVERIFY, OP_CHECKSIG)
    scriptPubKey.asm must be (expectedAsm)

  }
  it must "read a raw scriptPubKey from an output" in {
    //from b30d3148927f620f5b1228ba941c211fdabdae75d0ba0b688a58accbf018f3cc
    //output is index 1
    val rawScriptPubKey = "17a914af575bd77c5ce7eba3bd9ce6f89774713ae62c7987"
    val scriptPubKey = read(rawScriptPubKey)
    write(scriptPubKey) must be (rawScriptPubKey)
  }



}
