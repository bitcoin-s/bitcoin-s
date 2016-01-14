package org.scalacoin.marshallers.script

import org.scalacoin.protocol.script.ScriptSignature
import org.scalacoin.script.constant.ScriptConstantImpl
import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 1/12/16.
 */
class RawScriptSignatureParserTest extends FlatSpec with MustMatchers with RawScriptSignatureParser {

  //from bitcoin developer examples
  //https://bitcoin.org/en/developer-reference#raw-transaction-format
  val rawScriptSig = "494830450221008949f0cb400094ad2b5eb399d59d01c14d73d8fe6e96df1a7150deb388ab8935022079656090d7f6bac4c9a94e0aad311a4268e082a725f8aeae0573fb12ff866a5f01"
  "RawScriptSignatureParser" must "parse a raw scriptSig" in {
    val scriptSig : ScriptSignature = RawScriptSignatureParser.read(rawScriptSig)

    scriptSig.asm must be (Seq(ScriptConstantImpl("30450221008949f0cb400094ad2b5eb399d59d01c14d73d8fe6e96df1a7150deb388ab8935022079656090d7f6bac4c9a94e0aad311a4268e082a725f8aeae0573fb12ff866a5f01")))
    scriptSig.hex must be (rawScriptSig)
  }

  it must "write a raw script sig" in {
    val scriptSig = read(rawScriptSig)
    write(scriptSig) must be (rawScriptSig)
  }
}
