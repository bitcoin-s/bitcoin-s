package org.scalacoin.protocol.script

import org.scalacoin.crypto.{ECFactory}
import org.scalacoin.util.TestUtil
import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 3/8/16.
 */
class P2SHScriptSignatureTest extends FlatSpec with MustMatchers {

  "P2SHScriptSignature" must "find the public keys embedded inside of the redeemScript" in {
    val rawP2SHScriptSig = TestUtil.rawP2shInputScript2Of2
    val p2shScriptSig : P2SHScriptSignature = ScriptSignatureFactory.fromHex(rawP2SHScriptSig) match {
      case x : P2SHScriptSignature => x
      case y => throw new RuntimeException("Must be p2sh script sig: " + y)
    }
    p2shScriptSig.publicKeys must be (Seq(
      ECFactory.publicKey("0369d26ebd086523384a0f89f293d4c327a65fa73332d8efd1097cb35231295b83"),
      ECFactory.publicKey("02480863e5c4a4e9763f5380c44fcfe6a3b7787397076cf9ea1049303a9d34f721")
    ))

  }
}
