package org.bitcoins.core.script.interpreter

import java.io.File

import com.sun.org.apache.bcel.internal.generic.NOP
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.script.ScriptProgram
import org.bitcoins.core.script.bitwise.{OP_EQUAL, OP_EQUALVERIFY}
import org.bitcoins.core.script.constant._
import org.bitcoins.core.script.control.OP_VERIFY
import org.bitcoins.core.script.crypto.{OP_CHECKSIG, OP_HASH160}
import org.bitcoins.core.script.flag.ScriptFlagFactory
import org.bitcoins.core.script.interpreter.testprotocol.{CoreTestCase, CoreTestCaseProtocol}
import org.bitcoins.core.script.reserved.OP_NOP
import org.bitcoins.core.script.stack.OP_DUP
import org.bitcoins.core.util.{BitcoinSLogger, TestUtil, TransactionTestUtil}
import org.scalatest.{FlatSpec, MustMatchers}
import org.slf4j.LoggerFactory
import CoreTestCaseProtocol._
import org.bitcoins.core.protocol.transaction.testprotocol.CoreTransactionTestCase
import spray.json._

import scala.io.Source
/**
 * Created by chris on 1/6/16.
 */
class ScriptInterpreterTest extends FlatSpec with MustMatchers with ScriptInterpreter with BitcoinSLogger {

  "ScriptInterpreter" must "evaluate all the scripts from the bitcoin core script_tests.json" in {



    val source = Source.fromURL(getClass.getResource("/script_tests.json"))


    //use this to represent a single test case from script_valid.json
/*    val lines =
        """
          | [ ["NOP 0x01 1", "HASH160 0x14 0xda1745e9b549bd0bfa1a569971c77eba30cd5a4b EQUAL", "P2SH,STRICTENC", "SIG_PUSHONLY", "Tests for Script.IsPushOnly()"]]
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
