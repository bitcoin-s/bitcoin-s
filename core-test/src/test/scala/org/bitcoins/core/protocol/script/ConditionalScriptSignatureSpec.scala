package org.bitcoins.core.protocol.script

import org.bitcoins.testkit.core.gen.{NumberGenerator, ScriptGenerators}
import org.scalacheck.{Prop, Properties}

class ConditionalScriptSignatureSpec
    extends Properties("ConditionalScriptSignature") {
  property("Serialization symmetry") =
    Prop.forAll(ScriptGenerators.conditionalScriptSignature) {
      conditionalScriptSignature =>
        ConditionalScriptSignature(conditionalScriptSignature.bytes) == conditionalScriptSignature
    }

  property("Nesting agreement") =
    Prop.forAll(ScriptGenerators.scriptSignature, NumberGenerator.bool) {
      case (scriptSig, condition) =>
        val conditionalScriptSig =
          ConditionalScriptSignature(scriptSig, condition)

        conditionalScriptSig.nestedScriptSig == scriptSig
    }

  property("Signature agreement") =
    Prop.forAll(ScriptGenerators.conditionalScriptSignature) {
      conditionalScriptSignature =>
        conditionalScriptSignature.signatures == conditionalScriptSignature.nestedScriptSig.signatures
    }
}
