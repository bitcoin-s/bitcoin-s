package org.scalacoin.marshallers.script

import org.scalacoin.protocol.script.ScriptPubKey
import org.scalacoin.script.bitwise.OP_EQUALVERIFY
import org.scalacoin.script.constant.{BytesToPushOntoStackImpl, ScriptConstantImpl}
import org.scalacoin.script.crypto.{OP_CHECKSIG, OP_HASH160}
import org.scalacoin.script.stack.OP_DUP
import org.scalacoin.util.TestUtil
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

}
