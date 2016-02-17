package org.scalacoin.protocol.script

import org.scalacoin.script.crypto.SIGHASH_ALL
import org.scalacoin.util.{TestUtil, ScalacoinUtil}
import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 2/17/16.
 */
class ScriptSignatureTest extends FlatSpec with MustMatchers {

  "ScriptSignature" must "find the digital signature for the transaction inside of the script signature" in {
    //from this question
    //https://bitcoin.stackexchange.com/questions/37125/how-are-sighash-flags-encoded-into-a-signature
    val scriptSigHex = "304402206e3729f021476102a06ea453cea0a26cb9c096cca641efc4229c1111ed3a96fd022037dce1456a93f53d3e868c789b1b750a48a4c1110cd5b7049779b5f4f3c8b6200103ff1104b46b2141df1948dd0df2223720a3a471ec57404cace47063843a699a0f"

    val scriptSig = ScriptSignatureFactory.factory(scriptSigHex)

    scriptSig.signature must be ("304402206e3729f021476102a06ea453cea0a26cb9c096cca641efc4229c1111ed3a96fd022037dce1456a93f53d3e868c789b1b750a48a4c1110cd5b7049779b5f4f3c8b62001")
  }

/*   it must "derive the signature hash type from the signature" in {
    val signatureHex = "47304402206e3729f021476102a06ea453cea0a26cb9c096cca641efc4229c1111ed3a96fd022037dce1456a93f53d3e868c789b1b750a48a4c1110cd5b7049779b5f4f3c8b62001"

    val scriptSig = ScriptSignatureFactory.factory(signatureHex)


    scriptSig.hashType must be (SIGHASH_ALL)
  }



  it must "parse a p2pkh input script and derive it's hash type" in {
    val scriptSig = ScriptSignatureFactory.factory(TestUtil.p2pkhInputScript)
    scriptSig.hashType must be (SIGHASH_ALL)
  }*/


}
