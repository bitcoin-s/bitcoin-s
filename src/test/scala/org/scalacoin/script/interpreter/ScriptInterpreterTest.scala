package org.scalacoin.script.interpreter

import java.io.File

import org.scalacoin.script.bitwise.{OP_EQUAL, OP_EQUALVERIFY}
import org.scalacoin.script.constant._
import org.scalacoin.script.crypto.{OP_CHECKSIG, OP_HASH160}
import org.scalacoin.script.interpreter.testprotocol.{CoreTestCaseProtocol, CoreTestCase}
import org.scalacoin.script.stack.OP_DUP
import org.scalacoin.util.TestUtil
import org.scalatest.{MustMatchers, FlatSpec}
import org.slf4j.LoggerFactory

import spray.json._
/**
 * Created by chris on 1/6/16.
 */
class ScriptInterpreterTest extends FlatSpec with MustMatchers with ScriptInterpreter {


  private val logger = LoggerFactory.getLogger(this.getClass())

  "ScriptInterpreter" must "evaluate a valid script to true" in {
    //this is in asm format, not hex
    val inputScript = TestUtil.p2pkhInputScriptAsm
    //this is asm format, not hex
    val outputScript : List[ScriptToken] = TestUtil.p2pkhOutputScriptAsm
    val result = run(inputScript, outputScript)
    result must be (true)
  }


  it must "evaluate a script that asks to push 20 bytes onto the stack correctly" in {
    val stack = List(ScriptConstantImpl("68ca4fec736264c13b859bac43d5173df6871682"))
    val script = List(ScriptNumberImpl(20), ScriptConstantImpl("68ca4fec736264c13b859bac43d5173df6871682"), OP_EQUAL)

    run(stack,script) must be (true)
  }


  it must "evaluate all valid scripts from the bitcoin core script_valid.json" in {
    import CoreTestCaseProtocol._

    val source = scala.io.Source.fromFile("src/test/scala/org/scalacoin/script/interpreter/script_valid.json")

      //use this to represent a single test case from script_valid.json
      val lines =
      """
        |
        |[["0", "IF 1 IF RETURN ELSE RETURN ELSE RETURN ENDIF ELSE 1 IF 1 ELSE RETURN ELSE 1 ENDIF ELSE RETURN ENDIF ADD 2 EQUAL", "P2SH,STRICTENC", "Nested ELSE ELSE"]]
      """.stripMargin

    //val lines = try source.getLines.filterNot(_.isEmpty).map(_.trim) mkString "\n" finally source.close()
    val json = lines.parseJson
    val testCasesOpt : Seq[Option[CoreTestCase]] = json.convertTo[Seq[Option[CoreTestCase]]]
    val testCases : Seq[CoreTestCase] = testCasesOpt.flatten


    for {
      testCase <- testCases
    } yield {
      logger.info("Raw test case: " + testCase.raw)
      logger.info("Parsed ScriptSig: " + testCase.scriptSig)
      logger.info("Parsed ScriptPubKey: " + testCase.scriptPubKey)
      logger.info("Flags: " + testCase.flags)
      logger.info("Comments: " + testCase.comments)
      withClue(testCase.raw) {
        ScriptInterpreter.run(testCase.scriptSig, testCase.scriptPubKey) must equal (true)
      }
    }

  }








}
