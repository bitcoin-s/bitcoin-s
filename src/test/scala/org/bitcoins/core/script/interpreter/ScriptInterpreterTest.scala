package org.bitcoins.core.script.interpreter

import java.io.File

import com.sun.org.apache.bcel.internal.generic.NOP
import org.bitcoins.core.protocol.script.{EmptyScriptSignature, ScriptPubKey, ScriptSignature}
import org.bitcoins.core.script.ScriptProgram
import org.bitcoins.core.script.bitwise.{OP_EQUAL, OP_EQUALVERIFY}
import org.bitcoins.core.script.constant._
import org.bitcoins.core.script.control.OP_VERIFY
import org.bitcoins.core.script.crypto.{OP_CHECKSIG, OP_HASH160, SIGHASH_ALL}
import org.bitcoins.core.script.flag.{ScriptFlagFactory, ScriptVerifyP2SH}
import org.bitcoins.core.script.interpreter.testprotocol.{CoreTestCase, CoreTestCaseProtocol}
import org.bitcoins.core.script.reserved.OP_NOP
import org.bitcoins.core.script.stack.OP_DUP
import org.bitcoins.core.util._
import org.scalatest.{FlatSpec, MustMatchers}
import org.slf4j.LoggerFactory
import CoreTestCaseProtocol._
import org.bitcoins.core.crypto.{TransactionSignatureSerializer}
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.protocol.transaction.testprotocol.CoreTransactionTestCase
import org.bitcoins.core.script.result.ScriptOk
import spray.json._

import scala.io.Source
/**
 * Created by chris on 1/6/16.
 */
class ScriptInterpreterTest extends FlatSpec with MustMatchers with ScriptInterpreter with BitcoinSLogger {

  "ScriptInterpreter" must "evaluate all the scripts from the bitcoin core script_tests.json" in {

    val source = Source.fromURL(getClass.getResource("/script_tests.json"))


    //use this to represent a single test case from script_valid.json
    /*val lines =
        """
          | [[
          |    "0x48 0x3045022100e58c2307fb6569d0f25b4375f1074e48592bfc62d423bdc8f016365c980c0ae602204984523016f6320dc275cb90c6c1be80b4c9753de2e9fe19f2e5290844587da101 0x20 0x0228b426496c6a96846561a40b241ead0e03fe217d52de26cf2e707f0f181999fb 0x19 0x76a914364ddb17f9997cd91984c897eb5c0123f0a4b43f88ac",
          |    "HASH160 0x14 0x8a24f3c75f6d401a977bc63f854cc5d7dadfa7de EQUAL",
          |    "P2SH",
               "OK",
          |    "P2SH(P2PKH)"
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

}
