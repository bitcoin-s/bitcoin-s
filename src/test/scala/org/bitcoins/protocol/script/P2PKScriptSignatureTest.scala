package org.bitcoins.protocol.script

import org.bitcoins.crypto.ECFactory
import org.bitcoins.util.TestUtil
import org.scalatest.{MustMatchers, FlatSpec}

/**
 * Created by chris on 3/16/16.
 */
class P2PKScriptSignatureTest extends FlatSpec with MustMatchers {

  "P2PKScriptSignature" must "find the signature inside of a p2pk scriptSig" in {
    val p2pkScriptSig = TestUtil.p2pkScriptSig match {
      case s : P2PKScriptSignature => s
      case _ => throw new RuntimeException("SHould have been a p2pk scriptSig")
    }
    p2pkScriptSig.signature must be (ECFactory.digitalSignature("304402200a5c6163f07b8d3b013c4d1d6dba25e780b39658d79ba37af7057a3b7f15ffa102201fd9b4eaa9943f734928b99a83592c2e7bf342ea2680f6a2bb705167966b742001"))
  }
}
