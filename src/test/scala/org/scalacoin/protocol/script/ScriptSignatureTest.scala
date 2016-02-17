package org.scalacoin.protocol.script

import org.scalacoin.script.crypto.SIGHASH_ALL
import org.scalacoin.util.{TestUtil, ScalacoinUtil}
import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 2/17/16.
 */
class ScriptSignatureTest extends FlatSpec with MustMatchers {

  "ScriptSignature" must "find the digital signature for the transaction inside of a p2pkh script signature" in {
    val scriptSig = ScriptSignatureFactory.factory(TestUtil.rawScriptSig)
    scriptSig.signature.hex must be ("3045022100ad8e961fe3c22b2647d92b078f4c0cf81b3106ea5bf8b900ab8646aa4430216f022071d4edc2b5588be20ac4c2d07edd8ed069e10b2402d3dce2d3b835ccd075f28301")
  }

   it must "derive the signature hash type from the signature" in {

    TestUtil.scriptSig.hashType must be (SIGHASH_ALL)
  }


  it must "find the digital signature for a p2sh script signature" in {
    val scriptSig = TestUtil.p2shInputScript
    scriptSig.signature.hex must be ("304402207df6dd8dad22d49c3c83d8031733c32a53719278eb7985d3b35b375d776f84f102207054f9209a1e87d55feafc90aa04c33008e5bae9191da22aeaa16efde96f41f001")
  }

  it must "find the hash type for a p2sh script signature" in {
    TestUtil.p2shInputScript.hashType must be (SIGHASH_ALL)
  }


}
