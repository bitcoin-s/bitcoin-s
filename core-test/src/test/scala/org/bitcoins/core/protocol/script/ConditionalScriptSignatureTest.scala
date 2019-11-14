package org.bitcoins.core.protocol.script

import org.bitcoins.core.script.constant.{OP_FALSE, OP_TRUE}
import org.bitcoins.testkit.util.BitcoinSUnitTest

class ConditionalScriptSignatureTest extends BitcoinSUnitTest {

  behavior of "ConditionalScriptSignature"

  it should "correctly read true and false" in {
    val trueScriptSig = ConditionalScriptSignature.fromAsm(Vector(OP_TRUE))
    val falseScriptSig = ConditionalScriptSignature.fromAsm(Vector(OP_FALSE))

    assert(trueScriptSig.isTrue)
    assert(!trueScriptSig.isFalse)
    assert(!falseScriptSig.isTrue)
    assert(falseScriptSig.isFalse)
    assert(trueScriptSig.nestedScriptSig == EmptyScriptSignature)
    assert(falseScriptSig.nestedScriptSig == EmptyScriptSignature)
  }
}
