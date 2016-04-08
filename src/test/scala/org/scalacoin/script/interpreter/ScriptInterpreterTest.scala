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

import spray.json._
/**
 * Created by chris on 1/6/16.
 */
class ScriptInterpreterTest extends FlatSpec with MustMatchers with ScriptInterpreter {

  "ScriptInterpreter" must "evaluate all valid scripts from the bitcoin core script_valid.json" in {
    import CoreTestCaseProtocol._

    val source = scala.io.Source.fromFile("src/test/scala/org/scalacoin/script/interpreter/script_valid.json")

    //use this to represent a single test case from script_valid.json
/*    val lines =
        """
          |
          |[[
    "0x47 0x304402204e2eb034be7b089534ac9e798cf6a2c79f38bcb34d1b179efd6f2de0841735db022071461beb056b5a7be1819da6a3e3ce3662831ecc298419ca101eb6887b5dd6a401 0x19 0x76a9147cf9c846cd4882efec4bf07e44ebdad495c94f4b88ac",
    "HASH160 0x14 0x2df519943d5acc0ef5222091f9dfe3543f489a82 EQUAL",
    "",
    "P2SH(P2PKH), bad sig but no VERIFY_P2SH"
]]
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
    import CoreTestCaseProtocol._

    /**
     * These are test cases that were in script_valid.json that I have removed since i'm not sure how relevant
     * they are going forward to bitcoin  - for historical purposes though these should pass
     * they all have to do with DER encoded sigs
     * bitcoinj currently fails on these
     * ,
     */
/*    val source = scala.io.Source.fromFile("src/test/scala/org/scalacoin/script/interpreter/script_invalid.json")

    //use this to represent a single test case from script_valid.json
    val lines =
    """
      |
      |[["0x4c 01","0x01 NOP", "P2SH,STRICTENC", "PUSHDATA1 with not enough bytes"]]
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
        ScriptInterpreter.run(program) must equal (false)
      }
    }*/

  }
}
