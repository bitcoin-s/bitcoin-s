package org.bitcoins.core.protocol.script


import org.bitcoins.core.crypto._
import org.bitcoins.core.number.Int32
import org.bitcoins.core.protocol.script.testprotocol.SignatureHashTestCase
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.script.crypto.{HashType, SIGHASH_ALL, SIGHASH_SINGLE}
import org.bitcoins.core.serializers.script.RawScriptSignatureParser
import org.bitcoins.core.util.{BitcoinSLogger, BitcoinSUtil, TestUtil}
import org.scalatest.{FlatSpec, MustMatchers}
import spray.json._

import scala.io.Source

/**
 * Created by chris on 2/17/16.
 */
class ScriptSignatureTest extends FlatSpec with MustMatchers {
  private def logger = BitcoinSLogger.logger

  "ScriptSignature" must "find the digital signature for the transaction inside of a p2pkh script signature" in {
    val scriptSig = ScriptSignature(TestUtil.rawScriptSig)
    scriptSig.signatures.head.hex must be ("3045022100ad8e961fe3c22b2647d92b078f4c0cf81b3106ea5bf8b900ab8646aa4430216f022071d4edc2b5588be20ac4c2d07edd8ed069e10b2402d3dce2d3b835ccd075f28301")
  }


   it must "derive the signature hash type from the signature" in {
     HashType(Seq(TestUtil.scriptSig.signatures.head.bytes.last)) must be (HashType.sigHashAll)
  }


  it must "find the digital signature for a p2sh script signature" in {
    val scriptSig = TestUtil.p2shInputScript
    scriptSig.signatures.head.hex must be ("304402207df6dd8dad22d49c3c83d8031733c32a53719278eb7985d3b35b375d776f84f102207054f9209a1e87d55feafc90aa04c33008e5bae9191da22aeaa16efde96f41f001")
  }

  it must "find the digital signatures for a p2sh script signature for a 2/3 p2sh address" in {
    val scriptSig = TestUtil.p2shInputScript2Of2
    scriptSig.signatures must be (Seq(
      ECDigitalSignature("304402207d764cb90c9fd84b74d33a47cf3a0ffead9ded98333776becd6acd32c4426dac02203905a0d064e7f53d07793e86136571b6e4f700c1cfb888174e84d78638335b8101"),
      ECDigitalSignature("3045022100906aaca39f022acd8b7a38fd2f92aca9e9f35cfeaee69a6f13e1d083ae18222602204c9ed96fc6c4de56fd85c679fc59c16ee1ccc80c42563b86174e1a506fc007c801")
    ))
  }

  it must "find all the digital signatures for a p2sh script signature with a large amount of sigs" in {
    val scriptSig = TestUtil.p2shInputScriptLargeSignature

    scriptSig.signatures must be (Seq(
      ECDigitalSignature("3045022100a077d4fe9a81411ecb796c254d8b4e0bc73ff86a42288bc3b3ecfa1ef26c00dd02202389bf96cf38c14c3a6ccb8c688339f3fd880b724322862547a8ee3b547a9df901"),
      ECDigitalSignature("304402207c0692464998e7f3869f8501cdd25bbcd9d32b6fd34ae8aeae643b422a8dfd42022057eb16f8ca1f34e88babc9f8beb4c2521eb5c4dea41f8902a70d045f1c132a4401"),
      ECDigitalSignature("3044022024233923253c73569f4b34723a5495698bc124b099c5542a5997d13fba7d18a802203c317bddc070276c6f6c79cb3415413e608af30e4759e31b0d53eab3ca0acd4e01"),
      ECDigitalSignature("30450221009b9f0d8b945717d2fca3685093d547a3928d122b8894903ed51e2248303213bc022008b376422c9f2cd713b9d10b5b106d1c56c5893dcc01ae300253ed2234bdb63f01"),
      ECDigitalSignature("30440220257b57cb09386d82c4328461f8fe200c2f381d6b635e2a2f4ea40c8d945e9ec102201ec67d58d51a309af4d8896e9147a42944e9f9833a456f733ea5fa6954ed2fed01")
    ))
  }
  it must "find the hash type for a p2sh script signature" in {
    HashType(Seq(TestUtil.p2shInputScript2Of2.signatures.head.bytes.last)) must be (HashType.sigHashAll)
  }

  it must "find the digital signature and hash type for a SIGHASH_SINGLE" in {
    TestUtil.p2shInputScriptSigHashSingle.signatures.head.hex must be ("3045022100dfcfafcea73d83e1c54d444a19fb30d17317f922c19e2ff92dcda65ad09cba24022001e7a805c5672c49b222c5f2f1e67bb01f87215fb69df184e7c16f66c1f87c2903")
    HashType(TestUtil.p2shInputScriptSigHashSingle.signatures.head.bytes.last) must be (HashType.sigHashSingle)
  }

  it must "find the hash type for the weird occurrence of hash type being 0 on the blockchain" in {
    //from this tx https://btc.blockr.io/api/v1/tx/raw/c99c49da4c38af669dea436d3e73780dfdb6c1ecf9958baa52960e8baee30e73
    val hex = "8c493046022100d23459d03ed7e9511a47d13292d3430a04627de6235b6e51a40f9cd386f2abe3022100e7d25b080f0bb8d8d5f878bba7d54ad2fda650ea8d158a33ee3cbd11768191fd004104b0e2c879e4daf7b9ab68350228c159766676a14f5815084ba166432aab46198d4cca98fa3e9981d0a90b2effc514b76279476550ba3663fdcaff94c38420e9d5"
    val scriptSig : ScriptSignature = RawScriptSignatureParser.read(hex)
    HashType(scriptSig.signatures.head.bytes.last) must be (SIGHASH_ALL(Int32.zero))
  }

  it must "have an empty script signature" in {
    EmptyScriptSignature.hex must be ("00")
    EmptyScriptSignature.bytes must be (Seq(0.toByte))
    EmptyScriptSignature.asm must be (Nil)
    EmptyScriptSignature.signatures must be (Nil)
  }

  it must "create a p2pkh scriptSig" in {
    val scriptSig = ScriptSignature(TestUtil.p2pkhScriptSig.hex)
    scriptSig.isInstanceOf[P2PKHScriptSignature] must be (true)
    scriptSig.hex must be (TestUtil.p2pkhScriptSig.hex)
  }

  it must "create a p2sh scriptSig" in {
    val scriptSig = ScriptSignature(TestUtil.p2sh2Of3ScriptSig.hex)
    scriptSig.isInstanceOf[P2SHScriptSignature] must be (true)
    scriptSig.hex must be (TestUtil.p2sh2Of3ScriptSig.hex)
  }

  it must "create a p2pk scriptSig" in {
    val scriptSig = ScriptSignature(TestUtil.p2pkScriptSig.hex)
    scriptSig.isInstanceOf[P2PKScriptSignature] must be (true)
    scriptSig.hex must be (TestUtil.p2pkScriptSig.hex)
  }

  it must "read sighash.json and return result" in {
    import org.bitcoins.core.protocol.script.testprotocol.SignatureHashTestCaseProtocol._
    //["raw_transaction, script, input_index, hashType, signature_hash (result)"],
/*
    val lines =
      """
        | [
        | 	["a0aa3126041621a6dea5b800141aa696daf28408959dfb2df96095db9fa425ad3f427f2f6103000000015360290e9c6063fa26912c2e7fb6a0ad80f1c5fea1771d42f12976092e7a85a4229fdb6e890000000001abc109f6e47688ac0e4682988785744602b8c87228fcef0695085edf19088af1a9db126e93000000000665516aac536affffffff8fe53e0806e12dfd05d67ac68f4768fdbe23fc48ace22a5aa8ba04c96d58e2750300000009ac51abac63ab5153650524aa680455ce7b000000000000499e50030000000008636a00ac526563ac5051ee030000000003abacabd2b6fe000000000003516563910fb6b5", "65", 0, -1391424484, "48d6a1bd2cd9eec54eb866fc71209418a950402b5d7e52363bfb75c98e141175"]
        | ]
        |
      """.stripMargin
*/

    val source = Source.fromURL(this.getClass.getResource("/sighash.json"))
    val lines = try source.getLines.filterNot(_.isEmpty).map(_.trim).mkString("\n") finally source.close()
    val testCases : Seq[SignatureHashTestCase] = lines.parseJson.convertTo[Seq[SignatureHashTestCase]]

    for {
      testCase <- testCases
    } yield {
      Transaction(testCase.transaction.hex) must be (testCase.transaction)
      val hashForSig = TransactionSignatureSerializer.hashForSignature(testCase.transaction, testCase.inputIndex, testCase.script.asm, testCase.hashType)
      val flipHash = BitcoinSUtil.flipEndianness(testCase.hash.hex)
      hashForSig must be (DoubleSha256Digest(flipHash))
    }
  }

}
