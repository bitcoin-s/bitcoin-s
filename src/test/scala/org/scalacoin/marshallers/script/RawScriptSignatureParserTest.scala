package org.scalacoin.marshallers.script

import org.scalacoin.protocol.script.{ScriptSignatureFactory, ScriptSignature}
import org.scalacoin.script.constant.{OP_PUSHDATA1, ScriptConstantImpl, BytesToPushOntoStackImpl, OP_0}
import org.scalacoin.util.TestUtil
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

  it must "convert a raw script sig into the correct asm operations" in {

    val scriptSig = RawScriptSignatureParser.read(TestUtil.rawScriptSig)

    scriptSig.asm must be (Seq(BytesToPushOntoStackImpl(72), ScriptConstantImpl("3045022100ad8e961fe3c22b2647d92b078f4c0cf81b3106ea5bf8b900ab8646aa4430216f022071d4edc2b5588be20ac4c2d07edd8ed069e10b2402d3dce2d3b835ccd075f28301"), BytesToPushOntoStackImpl(65), ScriptConstantImpl("04fa79182bbc26c708b5d9f36b8635947d4a834ea356cf612ede08395c295f962e0b1dc2557aba34188640e51a58ed547f2c89c8265cd0c04ff890d8435648746e")))
  }


  it must "parse a raw scriptSig for a p2sh address with a lot of signatures" in  {
    TestUtil.p2shInputScriptLargeSignature.asm must be (Seq(OP_0,
      BytesToPushOntoStackImpl(72),
      ScriptConstantImpl("3045022100a077d4fe9a81411ecb796c254d8b4e0bc73ff86a42288bc3b3ecfa1ef26c00dd02202389bf96cf38c14c3a6ccb8c688339f3fd880b724322862547a8ee3b547a9df901"),
      BytesToPushOntoStackImpl(71),
      ScriptConstantImpl("304402207c0692464998e7f3869f8501cdd25bbcd9d32b6fd34ae8aeae643b422a8dfd42022057eb16f8ca1f34e88babc9f8beb4c2521eb5c4dea41f8902a70d045f1c132a4401"),
      BytesToPushOntoStackImpl(71),
      ScriptConstantImpl("3044022024233923253c73569f4b34723a5495698bc124b099c5542a5997d13fba7d18a802203c317bddc070276c6f6c79cb3415413e608af30e4759e31b0d53eab3ca0acd4e01"),
      BytesToPushOntoStackImpl(72),
      ScriptConstantImpl("30450221009b9f0d8b945717d2fca3685093d547a3928d122b8894903ed51e2248303213bc022008b376422c9f2cd713b9d10b5b106d1c56c5893dcc01ae300253ed2234bdb63f01"),
      BytesToPushOntoStackImpl(71),
      ScriptConstantImpl("30440220257b57cb09386d82c4328461f8fe200c2f381d6b635e2a2f4ea40c8d945e9ec102201ec67d58d51a309af4d8896e9147a42944e9f9833a456f733ea5fa6954ed2fed01"),
      OP_PUSHDATA1,
      BytesToPushOntoStackImpl(241),
      ScriptConstantImpl("55210269992fb441ae56968e5b77d46a3e53b69f136444ae65a94041fc937bdb28d93321021df31471281d4478df85bfce08a10aab82601dca949a79950f8ddf7002bd915a2102174c82021492c2c6dfcbfa4187d10d38bed06afb7fdcd72c880179fddd641ea121033f96e43d72c33327b6a4631ccaa6ea07f0b106c88b9dc71c9000bb6044d5e88a210313d8748790f2a86fb524579b46ce3c68fedd58d2a738716249a9f7d5458a15c221030b632eeb079eb83648886122a04c7bf6d98ab5dfb94cf353ee3e9382a4c2fab02102fb54a7fcaa73c307cfd70f3fa66a2e4247a71858ca731396343ad30c7c4009ce57ae")

    )
    )

  }

  it must "read a empty script sig" in {
    val emptyScriptSig = ScriptSignatureFactory.empty
    val parsedScriptSig = RawScriptSignatureParser.read(emptyScriptSig.hex)

    parsedScriptSig.hex must be (emptyScriptSig.hex)
  }
}
