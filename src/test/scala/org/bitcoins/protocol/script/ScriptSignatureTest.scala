package org.bitcoins.protocol.script


import org.bitcoins.crypto.ECFactory
import org.bitcoins.marshallers.script.RawScriptSignatureParser
import org.bitcoins.script.constant._
import org.bitcoins.script.crypto.{SIGHASH_SINGLE, SIGHASH_ALL}
import org.bitcoins.util.{BitcoinScriptUtil, TransactionTestUtil, TestUtil}
import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 2/17/16.
 */
class ScriptSignatureTest extends FlatSpec with MustMatchers {

  "ScriptSignature" must "find the digital signature for the transaction inside of a p2pkh script signature" in {
    val scriptSig = ScriptSignature(TestUtil.rawScriptSig)
    scriptSig.signatures.head.hex must be ("3045022100ad8e961fe3c22b2647d92b078f4c0cf81b3106ea5bf8b900ab8646aa4430216f022071d4edc2b5588be20ac4c2d07edd8ed069e10b2402d3dce2d3b835ccd075f28301")
  }


   it must "derive the signature hash type from the signature" in {
    TestUtil.scriptSig.hashType(TestUtil.scriptSig.signatures.head) must be (SIGHASH_ALL())
  }


  it must "find the digital signature for a p2sh script signature" in {
    val scriptSig = TestUtil.p2shInputScript
    scriptSig.signatures.head.hex must be ("304402207df6dd8dad22d49c3c83d8031733c32a53719278eb7985d3b35b375d776f84f102207054f9209a1e87d55feafc90aa04c33008e5bae9191da22aeaa16efde96f41f001")
  }

  it must "find the digital signatures for a p2sh script signature for a 2/3 p2sh address" in {
    val scriptSig = TestUtil.p2shInputScript2Of2
    scriptSig.signatures must be (Seq(
      ECFactory.digitalSignature("304402207d764cb90c9fd84b74d33a47cf3a0ffead9ded98333776becd6acd32c4426dac02203905a0d064e7f53d07793e86136571b6e4f700c1cfb888174e84d78638335b8101"),
      ECFactory.digitalSignature("3045022100906aaca39f022acd8b7a38fd2f92aca9e9f35cfeaee69a6f13e1d083ae18222602204c9ed96fc6c4de56fd85c679fc59c16ee1ccc80c42563b86174e1a506fc007c801")
    ))
  }

  it must "find all the digital signatures for a p2sh script signature with a large amount of sigs" in {
    val scriptSig = TestUtil.p2shInputScriptLargeSignature

    scriptSig.signatures must be (Seq(
      ECFactory.digitalSignature("3045022100a077d4fe9a81411ecb796c254d8b4e0bc73ff86a42288bc3b3ecfa1ef26c00dd02202389bf96cf38c14c3a6ccb8c688339f3fd880b724322862547a8ee3b547a9df901"),
      ECFactory.digitalSignature("304402207c0692464998e7f3869f8501cdd25bbcd9d32b6fd34ae8aeae643b422a8dfd42022057eb16f8ca1f34e88babc9f8beb4c2521eb5c4dea41f8902a70d045f1c132a4401"),
      ECFactory.digitalSignature("3044022024233923253c73569f4b34723a5495698bc124b099c5542a5997d13fba7d18a802203c317bddc070276c6f6c79cb3415413e608af30e4759e31b0d53eab3ca0acd4e01"),
      ECFactory.digitalSignature("30450221009b9f0d8b945717d2fca3685093d547a3928d122b8894903ed51e2248303213bc022008b376422c9f2cd713b9d10b5b106d1c56c5893dcc01ae300253ed2234bdb63f01"),
      ECFactory.digitalSignature("30440220257b57cb09386d82c4328461f8fe200c2f381d6b635e2a2f4ea40c8d945e9ec102201ec67d58d51a309af4d8896e9147a42944e9f9833a456f733ea5fa6954ed2fed01")
    ))
  }
  it must "find the hash type for a p2sh script signature" in {
    TestUtil.p2shInputScript.hashType(TestUtil.p2shInputScript2Of2.signatures.head) must be (SIGHASH_ALL())
  }

  it must "find the digital signature and hash type for a SIGHASH_SINGLE" in {
    TestUtil.p2shInputScriptSigHashSingle.signatures.head.hex must be ("3045022100dfcfafcea73d83e1c54d444a19fb30d17317f922c19e2ff92dcda65ad09cba24022001e7a805c5672c49b222c5f2f1e67bb01f87215fb69df184e7c16f66c1f87c2903")
    TestUtil.p2shInputScriptSigHashSingle.hashType(TestUtil.p2shInputScriptSigHashSingle.signatures.head) must be (SIGHASH_SINGLE)
  }

  it must "find the hash type for the weird occurrence of hash type being 0 on the blockchain" in {
    //from this tx https://btc.blockr.io/api/v1/tx/raw/c99c49da4c38af669dea436d3e73780dfdb6c1ecf9958baa52960e8baee30e73
    val hex = "493046022100d23459d03ed7e9511a47d13292d3430a04627de6235b6e51a40f9cd386f2abe3022100e7d25b080f0bb8d8d5f878bba7d54ad2fda650ea8d158a33ee3cbd11768191fd004104b0e2c879e4daf7b9ab68350228c159766676a14f5815084ba166432aab46198d4cca98fa3e9981d0a90b2effc514b76279476550ba3663fdcaff94c38420e9d5"
    val scriptSig : ScriptSignature = RawScriptSignatureParser.read(hex)
    scriptSig.hashType(scriptSig.signatures.head) must be (SIGHASH_ALL(0))
  }

  it must "have an empty script signature" in {
    EmptyScriptSignature.hex must be ("")
    EmptyScriptSignature.bytes must be (Seq())
    EmptyScriptSignature.asm must be (Seq())
    EmptyScriptSignature.signatures must be (Seq())
  }

  it must "use the p2pkh companion object to create a p2pkh scriptSig" in {
    P2PKHScriptSignatureImpl(TestUtil.p2pkScriptSig.hex).hex must be (TestUtil.p2pkScriptSig.hex)
  }

  it must "use the p2sh companion object to create a p2sh scriptSig" in {
    P2SHScriptSignatureImpl(TestUtil.p2sh2Of3ScriptSig.hex).hex must be (TestUtil.p2sh2Of3ScriptSig.hex)
  }

  it must "use the p2pk companion object to create a p2pk scriptSig" in {
    P2PKScriptSignatureImpl(TestUtil.p2pkScriptSig.hex).hex must be (TestUtil.p2pkScriptSig.hex)
  }

  it must "use the nonstandard scriptSig companion object to create a nonstandard scriptSig" in {
    NonStandardScriptSignatureImpl(TestUtil.p2pkScriptSig.hex).hex must be (TestUtil.p2pkScriptSig.hex)
  }



}
