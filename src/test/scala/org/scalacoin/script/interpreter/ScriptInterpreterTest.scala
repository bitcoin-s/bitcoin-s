package org.scalacoin.script.interpreter

import java.io.File

import com.sun.org.apache.bcel.internal.generic.NOP
import org.scalacoin.protocol.script.{ScriptPubKeyFactory, ScriptPubKey}
import org.scalacoin.script.ScriptProgramFactory
import org.scalacoin.script.bitwise.{OP_EQUAL, OP_EQUALVERIFY}
import org.scalacoin.script.constant._
import org.scalacoin.script.control.OP_VERIFY
import org.scalacoin.script.crypto.{OP_CHECKSIG, OP_HASH160}
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

  it must "evaluate a NOP correctly" in {
    val stack = List()
    val script = List(OP_NOP)
    val program = ScriptProgramFactory.factory(TestUtil.testProgram,stack,script)
    run(program) must equal (true)

  }




  it must "evaluate all valid scripts from the bitcoin core script_valid.json" in {
    import CoreTestCaseProtocol._

    val source = scala.io.Source.fromFile("src/test/scala/org/scalacoin/script/interpreter/script_valid.json")

    //use this to represent a single test case from script_valid.json
    val lines =
    """
      |
      |[["0x4a 0x0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000", "0 CHECKSIG NOT", "", "Overly long signature is correctly encoded"]]
    """.stripMargin

    //val lines = try source.getLines.filterNot(_.isEmpty).map(_.trim) mkString "\n" finally source.close()
    val json = lines.parseJson
    val testCasesOpt : Seq[Option[CoreTestCase]] = json.convertTo[Seq[Option[CoreTestCase]]]
    val testCases : Seq[CoreTestCase] = testCasesOpt.flatten


    for {
      testCase <- testCases
      creditingTx = TransactionTestUtil.buildCreditingTransaction(testCase.scriptPubKey.scriptPubKey)
      tx = TransactionTestUtil.buildSpendingTransaction(creditingTx,testCase.scriptSig.scriptSignature,0)
    } yield {
      require(testCase.scriptPubKey.asm == testCase.scriptPubKey.scriptPubKey.asm)
      logger.info("Raw test case: " + testCase.raw)
      logger.info("Parsed ScriptSig: " + testCase.scriptSig)
      logger.info("Parsed ScriptPubKey: " + testCase.scriptPubKey)
      logger.info("Flags: " + testCase.flags)
      logger.info("Comments: " + testCase.comments)
      val scriptPubKey = ScriptPubKeyFactory.fromAsm(testCase.scriptPubKey.asm)
      val inputIndex = 0
      val program = ScriptProgramFactory.factory(tx,scriptPubKey, inputIndex)
      withClue(testCase.raw) {
        ScriptInterpreter.run(program) must equal (true)
      }
    }

  }
}
