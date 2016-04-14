package org.scalacoin.script.interpreter

import java.io.File

import com.sun.org.apache.bcel.internal.generic.NOP
import org.scalacoin.protocol.script.{ScriptPubKeyFactory, ScriptPubKey}
import org.scalacoin.script.ScriptProgramFactory
import org.scalacoin.script.bitwise.{OP_EQUAL, OP_EQUALVERIFY}
import org.scalacoin.script.constant._
import org.scalacoin.script.control.OP_VERIFY
import org.scalacoin.script.crypto.{OP_CHECKSIG, OP_HASH160}
import org.scalacoin.script.flag.ScriptFlagFactory
import org.scalacoin.script.interpreter.testprotocol.{CoreTestCaseProtocol, CoreTestCase}
import org.scalacoin.script.reserved.OP_NOP
import org.scalacoin.script.stack.OP_DUP
import org.scalacoin.util.{BitcoinSLogger, TransactionTestUtil, TestUtil}
import org.scalatest.{MustMatchers, FlatSpec}
import org.slf4j.LoggerFactory
import CoreTestCaseProtocol._
import spray.json._
/**
 * Created by chris on 1/6/16.
 */
class ScriptInterpreterTest extends FlatSpec with MustMatchers with ScriptInterpreter with BitcoinSLogger {

  "ScriptInterpreter" must "evaluate all valid scripts from the bitcoin core script_valid.json" in {


    val source = scala.io.Source.fromFile("src/test/scala/org/scalacoin/script/interpreter/script_valid.json")

    //use this to represent a single test case from script_valid.json
    val lines =
        """
          |
          |[["''", "RIPEMD160 0x14 0x9c1185a5c5e9fc54612808977ee8f548b2258d31 EQUAL", "P2SH,STRICTENC"]]
   """.stripMargin
    //val lines = try source.getLines.filterNot(_.isEmpty).map(_.trim) mkString "\n" finally source.close()
    val json = lines.parseJson
    val testCasesOpt : Seq[Option[CoreTestCase]] = json.convertTo[Seq[Option[CoreTestCase]]]
    val testCases : Seq[CoreTestCase] = testCasesOpt.flatten


    for {
      testCase <- testCases
      (creditingTx,outputIndex) = TransactionTestUtil.buildCreditingTransaction(testCase.scriptPubKey.scriptPubKey)
      (tx,inputIndex) = TransactionTestUtil.buildSpendingTransaction(creditingTx,testCase.scriptSig.scriptSignature,outputIndex)
    } yield {
      require(testCase.scriptPubKey.asm == testCase.scriptPubKey.scriptPubKey.asm)
      logger.info("Raw test case: " + testCase.raw)
      logger.info("Parsed ScriptSig: " + testCase.scriptSig)
      logger.info("Parsed ScriptPubKey: " + testCase.scriptPubKey)
      logger.info("Flags: " + testCase.flags)
      logger.info("Comments: " + testCase.comments)
      val scriptPubKey = ScriptPubKeyFactory.fromAsm(testCase.scriptPubKey.asm)
      val flags = ScriptFlagFactory.fromList(testCase.flags)
      logger.info("Flags after parsing: " + flags)
      val program = ScriptProgramFactory.factory(tx,scriptPubKey,inputIndex,flags)
      withClue(testCase.raw) {
        ScriptInterpreter.run(program) must equal (true)
      }
    }
  }

  it must "evaluate all valid scripts from the bitcoin core script_invalid.json" in {
    val source = scala.io.Source.fromFile("src/test/scala/org/scalacoin/script/interpreter/script_invalid.json")

    //use this to represent a single test case from script_valid.json
    val lines =
    """
      |
      |[["0",
      |"IF 0x6161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161 ENDIF 1",
      |"P2SH,STRICTENC",
      |">201 opcodes including non-executed IF branch. 0x61 is NOP"]]
    """.stripMargin

    //val lines = try source.getLines.filterNot(_.isEmpty).map(_.trim) mkString "\n" finally source.close()
/*    val json = lines.parseJson
    val testCasesOpt : Seq[Option[CoreTestCase]] = json.convertTo[Seq[Option[CoreTestCase]]]
    val testCases : Seq[CoreTestCase] = testCasesOpt.flatten


    for {
      testCase <- testCases
      (creditingTx,outputIndex) = TransactionTestUtil.buildCreditingTransaction(testCase.scriptPubKey.scriptPubKey)
      (tx,inputIndex) = TransactionTestUtil.buildSpendingTransaction(creditingTx,testCase.scriptSig.scriptSignature,outputIndex)
    } yield {

      require(testCase.scriptPubKey.asm == testCase.scriptPubKey.scriptPubKey.asm)

      logger.info("Raw test case: " + testCase.raw)
      logger.info("Parsed ScriptSig: " + testCase.scriptSig)
      logger.info("Parsed ScriptPubKey: " + testCase.scriptPubKey)
      logger.info("Flags: " + testCase.flags)
      logger.info("Comments: " + testCase.comments)
      val scriptPubKey = ScriptPubKeyFactory.fromAsm(testCase.scriptPubKey.asm)
      val flags = ScriptFlagFactory.fromList(testCase.flags)
      logger.info("Flags after parsing: " + flags)
      val program = ScriptProgramFactory.factory(tx,scriptPubKey,inputIndex,flags)
      withClue(testCase.raw) {
        ScriptInterpreter.run(program) must equal (false)
      }
    }*/

  }
}
