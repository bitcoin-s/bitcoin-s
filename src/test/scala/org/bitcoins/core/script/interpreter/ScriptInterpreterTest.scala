package org.bitcoins.core.script.interpreter


import org.bitcoins.core.crypto.{TransactionSignatureComponent, ECPrivateKey}
import org.bitcoins.core.gen.TransactionGenerators
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script.{P2SHScriptSignature, CLTVScriptSignature, CLTVScriptPubKey, ScriptPubKey}
import org.bitcoins.core.script.ScriptProgram
import org.bitcoins.core.script.flag.ScriptFlagFactory
import org.bitcoins.core.script.interpreter.testprotocol.CoreTestCaseProtocol._
import org.bitcoins.core.script.interpreter.testprotocol.{CoreTestCase, CoreTestCaseProtocol}
import org.bitcoins.core.script.result.ScriptErrorUnsatisfiedLocktime
import org.bitcoins.core.util._
import org.scalatest.{FlatSpec, MustMatchers}
import spray.json._

import scala.io.Source
/**
 * Created by chris on 1/6/16.
 */
class ScriptInterpreterTest extends FlatSpec with MustMatchers with ScriptInterpreter with BitcoinSLogger {
/*

  "ScriptInterpreter" must "evaluate all the scripts from the bitcoin core script_tests.json" in {

    val source = Source.fromURL(getClass.getResource("/script_tests.json"))


    //use this to represent a single test case from script_valid.json
/*    val lines =
        """
          | [[
          |    "0x47 0x304402204710a85181663b32d25c70ec2bbd14adff5ddfff6cb50d09e155ef5f541fc86c0220056b0cc949be9386ecc5f6c2ac0493269031dbb185781db90171b54ac127790281",
          |  "0x41 0x048282263212c609d9ea2a6e3e172de238d8c39cabd5ac1ca10646e23fd5f5150811f8a8098557dfe45e8256e830b60ace62d613ac2f7b17bed31b6eaff6e26caf CHECKSIG",
          |  "",
          |  "OK",
          |  "P2PK anyonecanpay"
          |]]
   """.stripMargin*/
    val lines = try source.getLines.filterNot(_.isEmpty).map(_.trim) mkString "\n" finally source.close()
    val json = lines.parseJson
    val testCasesOpt : Seq[Option[CoreTestCase]] = json.convertTo[Seq[Option[CoreTestCase]]]
    val testCases : Seq[CoreTestCase] = testCasesOpt.flatten
    for {
      testCase <- testCases
      (creditingTx,outputIndex) = TransactionTestUtil.buildCreditingTransaction(testCase.scriptPubKey)
      (tx,inputIndex) = TransactionTestUtil.buildSpendingTransaction(creditingTx,testCase.scriptSig,outputIndex)
    } yield {
      logger.info("Raw test case: " + testCase.raw)
      logger.info("Parsed ScriptSig: " + testCase.scriptSig)
      logger.info("Parsed ScriptPubKey: " + testCase.scriptPubKey)
      logger.info("Flags: " + testCase.flags)
      logger.info("Comments: " + testCase.comments)
      val scriptPubKey = ScriptPubKey.fromAsm(testCase.scriptPubKey.asm)
      val flags = ScriptFlagFactory.fromList(testCase.flags)
      logger.info("Flags after parsing: " + flags)
      val program = ScriptProgram(tx,scriptPubKey,inputIndex,flags)
      withClue(testCase.raw) {
        ScriptInterpreter.run(program) must equal (testCase.expectedResult)
      }
    }
  }
*/

  it must "fail CLTVScriptPubKey with nested P2SHScriptPubKey" in {
    val privKey = ECPrivateKey("8fe79310a9a5400daf4f9690187db1607c668fca9f63a530d67cc7472c9de589")
    val cltv = CLTVScriptPubKey("010ab175a914c280ffb6c00e2d8b333d84746dc38a807081590287")
    val p2shScriptSig = P2SHScriptSignature("483045022100d817fbc5a5d71f0859b811c01e251a20ab897c2b07b96149340336183e80d1" +
      "9e022065e20731db313b31c45b208919a8606f90d390620b65a71085d185618b6742bc0123210338a011f3d144d4af991b27d13bcac38ef312835e64a0971c7f5404e1252a332eac")
    val cltvScriptSignature = CLTVScriptSignature(p2shScriptSig)
    val redeemScript = p2shScriptSig.redeemScript
    val (creditingTx, outputIndex) = TransactionGenerators.buildCreditingTransaction(cltv)
    val (spendingTx, inputIndex) = TransactionGenerators.buildSpendingTransaction(UInt32.one, creditingTx, cltvScriptSignature, outputIndex, UInt32(5), UInt32.zero)
    val txSigComp = TransactionSignatureComponent(spendingTx, inputIndex, redeemScript, Policy.standardScriptVerifyFlags)
    ScriptInterpreter.run(ScriptProgram(txSigComp)) must be (ScriptErrorUnsatisfiedLocktime)
  }

}
