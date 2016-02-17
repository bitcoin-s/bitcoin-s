package org.scalacoin.protocol.script

import org.scalacoin.script.crypto.SIGHASH_ALL
import org.scalacoin.util.{TestUtil, ScalacoinUtil}
import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 2/17/16.
 */
class ScriptSignatureTest extends FlatSpec with MustMatchers {

  "ScriptSignature" must "find the digital signature for the transaction inside of the script signature" in {
    val scriptSig = ScriptSignatureFactory.factory(TestUtil.rawScriptSig)
    scriptSig.signature.head.hex must be ("3045022100ad8e961fe3c22b2647d92b078f4c0cf81b3106ea5bf8b900ab8646aa4430216f022071d4edc2b5588be20ac4c2d07edd8ed069e10b2402d3dce2d3b835ccd075f28301")
  }

   it must "derive the signature hash type from the signature" in {

    TestUtil.scriptSig.hashType must be (SIGHASH_ALL)
  }



  /*it must "parse a p2pkh input script and derive it's hash type" in {
    val scriptSig = ScriptSignatureFactory.factory(TestUtil.p2pkhInputScript)
    scriptSig.hashType must be (SIGHASH_ALL)
  }*/


}
