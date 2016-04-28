package org.bitcoins.script.interpreter

import java.io.File

import com.sun.org.apache.bcel.internal.generic.NOP
import org.bitcoins.protocol.script.{ScriptPubKey}
import org.bitcoins.script.ScriptProgram
import org.bitcoins.script.bitwise.{OP_EQUAL, OP_EQUALVERIFY}
import org.bitcoins.script.constant._
import org.bitcoins.script.control.OP_VERIFY
import org.bitcoins.script.crypto.{OP_CHECKSIG, OP_HASH160}
import org.bitcoins.script.flag.ScriptFlagFactory
import org.bitcoins.script.interpreter.testprotocol.{CoreTestCaseProtocol, CoreTestCase}
import org.bitcoins.script.reserved.OP_NOP
import org.bitcoins.script.stack.OP_DUP
import org.bitcoins.util.{BitcoinSLogger, TransactionTestUtil, TestUtil}
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
/*    val lines =
        """
          |
          |[["1",
"0x616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161",
"P2SH,STRICTENC",
"201 opcodes executed. 0x61 is NOP"]]
   """.stripMargin*/
    val lines = try source.getLines.filterNot(_.isEmpty).map(_.trim) mkString "\n" finally source.close()
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
      val scriptPubKey = ScriptPubKey.fromAsm(testCase.scriptPubKey.asm)
      val flags = ScriptFlagFactory.fromList(testCase.flags)
      logger.info("Flags after parsing: " + flags)
      val program = ScriptProgram(tx,scriptPubKey,inputIndex,flags)
      withClue(testCase.raw) {
        ScriptInterpreter.run(program) must equal (true)
      }
    }
  }

  it must "evaluate all valid scripts from the bitcoin core script_invalid.json" in {
    val source = scala.io.Source.fromFile("src/test/scala/org/scalacoin/script/interpreter/script_invalid.json")

    //use this to represent a single test case from script_valid.json
/*    val lines =
    """
      |[[
      |    "0x47 0x3044022003fef42ed6c7be8917441218f525a60e2431be978e28b7aca4d7a532cc413ae8022067a1f82c74e8d69291b90d148778405c6257bbcfc2353cc38a3e1f22bf44254601 0x23 0x210279be667ef9dcbbac54a06295ce870b07029bfcdb2dce28d959f2815b16f81798ac",
      |    "HASH160 0x14 0x23b0ad3477f2178bc0b3eed26e4e6316f4e83aa1 EQUAL",
      |    "P2SH",
      |    "P2SH(P2PK), bad redeemscript"
      |]]
    """.stripMargin*/

    val lines = try source.getLines.filterNot(_.isEmpty).map(_.trim) mkString "\n" finally source.close()
    val json = lines.parseJson
    val testCasesOpt : Seq[Option[CoreTestCase]] = json.convertTo[Seq[Option[CoreTestCase]]]
    val testCases : Seq[CoreTestCase] = testCasesOpt.flatten


    for {
      testCase <- testCases
      (creditingTx,outputIndex) = TransactionTestUtil.buildCreditingTransaction(testCase.scriptPubKey.scriptPubKey)
      (tx,inputIndex) = TransactionTestUtil.buildSpendingTransaction(creditingTx,testCase.scriptSig.scriptSignature,outputIndex)
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
        ScriptInterpreter.run(program) must equal (false)
      }
    }

  }
}
