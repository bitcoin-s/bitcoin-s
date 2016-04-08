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
class ScriptInterpreterTest extends FlatSpec with MustMatchers with ScriptInterpreter with BitcoinSLogger {

  "ScriptInterpreter" must "evaluate all valid scripts from the bitcoin core script_valid.json" in {
    import CoreTestCaseProtocol._

    val source = scala.io.Source.fromFile("src/test/scala/org/scalacoin/script/interpreter/script_valid.json")

    //use this to represent a single test case from script_valid.json
/*    val lines =
        """
          |
          |[[
    "0 0x47 0x304402205b7d2c2f177ae76cfbbf14d589c113b0b35db753d305d5562dd0b61cbf366cfb02202e56f93c4f08a27f986cd424ffc48a462c3202c4902104d4d0ff98ed28f4bf8001 0x47 0x30440220563e5b3b1fc11662a84bc5ea2a32cc3819703254060ba30d639a1aaf2d5068ad0220601c1f47ddc76d93284dd9ed68f7c9974c4a0ea7cbe8a247d6bc3878567a5fca01 0x4c 0x69 0x52210279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f8179821038282263212c609d9ea2a6e3e172de238d8c39cabd5ac1ca10646e23fd5f515082103363d90d447b00c9c99ceac05b6262ee053441c7e55552ffe526bad8f83ff464053ae",
    "HASH160 0x14 0xc9e4a896d149702d0d1695434feddd52e24ad78d EQUAL",
    "P2SH",
    "P2SH(2-of-3)"
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
    val source = scala.io.Source.fromFile("src/test/scala/org/scalacoin/script/interpreter/script_invalid.json")

    //use this to represent a single test case from script_valid.json
/*        val lines =
        """
          |
          |[["", "DEPTH", "P2SH,STRICTENC",   "Test the test: we should have an empty stack after scriptSig evaluation"]]
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
        ScriptInterpreter.run(program) must equal (false)
      }
    }

  }
}
