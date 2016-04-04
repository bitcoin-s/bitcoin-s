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

  "ScriptInterpreter" must "evaluate a valid script to true" in {
    //this is in asm format, not hex
    val inputScript = TestUtil.p2pkhInputScriptAsm
    //this is asm format, not hex
    val outputScript : List[ScriptToken] = TestUtil.p2pkhOutputScriptAsm
    val stack = List()
    val script = inputScript ++ outputScript
    val program = ScriptProgramFactory.factory(TestUtil.testProgram,stack,script)
    val result = run(program)
    result must be (true)
  }


  it must "evaluate a script that asks to push 20 bytes onto the stack correctly" in {
    val stack = List(ScriptConstantImpl("68ca4fec736264c13b859bac43d5173df6871682"))
    val script = List(BytesToPushOntoStackImpl(20), ScriptConstantImpl("68ca4fec736264c13b859bac43d5173df6871682"), OP_EQUAL)

    val program = ScriptProgramFactory.factory(TestUtil.testProgram,stack,script)
    run(program) must be (true)
  }

  it must "evaluate a 5 byte representation of 0x0000000001 as 0x01 when pushed onto the stack" in {
    //this is for the following test case inside of script_valid.json
    //["1 0x05 0x01 0x00 0x00 0x00 0x00", "VERIFY", "P2SH,STRICTENC", "values >4 bytes can be cast to boolean"]
    val stack = List(OP_1)
    val script = List(BytesToPushOntoStackImpl(5), ScriptNumberImpl(1), OP_0, OP_0, OP_0, OP_0,OP_VERIFY)
    val program = ScriptProgramFactory.factory(TestUtil.testProgram,stack,script)
    run(program) must equal (true)
  }



  it must "evaluate all valid scripts from the bitcoin core script_valid.json" in {
    import CoreTestCaseProtocol._

    val source = scala.io.Source.fromFile("src/test/scala/org/scalacoin/script/interpreter/script_valid.json")

    //use this to represent a single test case from script_valid.json
/*    val lines =
        """
          |
          |[[
    "0x47 0x3044022003fef42ed6c7be8917441218f525a60e2431be978e28b7aca4d7a532cc413ae8022067a1f82c74e8d69291b90d148778405c6257bbcfc2353cc38a3e1f22bf44254601 0x23 0x210279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798ac",
    "HASH160 0x14 0x23b0ad3477f2178bc0b3eed26e4e6316f4e83aa1 EQUAL",
    "P2SH",
    "P2SH(P2PK)"
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

/*  it must "evaluate all valid scripts from the bitcoin core script_invalid.json" in {
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

  }*/
}
