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

  "RawScriptSignatureParser" must "write a raw script sig" in {
    val scriptSig = read(rawScriptSig)
    write(scriptSig) must be (rawScriptSig)
  }

  it must "read then write a raw script sig" in {
    //from this tx
    //https://tbtc.blockr.io/api/v1/tx/raw/bdc221db675c06dbee2ae75d33e31cad4e2555efea10c337ff32c8cdf97f8e74
    val rawScriptSig = "483045022100ad8e961fe3c22b2647d92b078f4c0cf81b3106ea5bf8b900ab8646aa4430216f022071d4edc2b5588be20ac4c2d07edd8ed069e10b2402d3dce2d3b835ccd075f283014104fa79182bbc26c708b5d9f36b8635947d4a834ea356cf612ede08395c295f962e0b1dc2557aba34188640e51a58ed547f2c89c8265cd0c04ff890d8435648746e"
    val scriptSig = RawScriptSignatureParser.read(rawScriptSig)
    RawScriptSignatureParser.write(scriptSig) must be (rawScriptSig)
  }
}
