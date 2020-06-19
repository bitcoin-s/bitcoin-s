package org.bitcoins.core.protocol.script

import org.bitcoins.core.script.constant.{OP_FALSE, OP_TRUE}
import org.bitcoins.testkit.core.gen.{NumberGenerator, ScriptGenerators}
import org.bitcoins.testkit.util.BitcoinSUnitTest

class ConditionalScriptSignatureTest extends BitcoinSUnitTest {
  behavior of "ConditionalScriptSignature"

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration = {
    generatorDrivenConfigNewCode
  }

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

  it should "have serialization symmetry" in {
    forAll(ScriptGenerators.conditionalScriptSignature) {
      conditionalScriptSignature =>
        assert(
          ConditionalScriptSignature(
            conditionalScriptSignature.bytes) == conditionalScriptSignature)
    }
  }

  it should "have agreement with nesting ScriptSignatures" in {
    forAll(ScriptGenerators.scriptSignature, NumberGenerator.bool) {
      case (scriptSig, condition) =>
        val conditionalScriptSig =
          ConditionalScriptSignature(scriptSig, condition)

        assert(conditionalScriptSig.nestedScriptSig == scriptSig)
    }
  }

  it should "have agreement with nested signatures" in {
    forAll(ScriptGenerators.conditionalScriptSignature) {
      conditionalScriptSignature =>
        assert(
          conditionalScriptSignature.signatures == conditionalScriptSignature.nestedScriptSig.signatures)
    }
  }
}
