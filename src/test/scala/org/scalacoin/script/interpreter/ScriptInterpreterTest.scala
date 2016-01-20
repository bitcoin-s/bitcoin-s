package org.scalacoin.script.interpreter

import java.io.File

import org.scalacoin.script.bitwise.OP_EQUALVERIFY
import org.scalacoin.script.constant.ScriptToken
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


  it must "evaluate all valid scripts from the bitcoin core script_valid.json" in {
    import CoreTestCaseProtocol._

    val source = scala.io.Source.fromFile("src/test/scala/org/scalacoin/script/interpreter/script_valid.json")
    val lines = try source.getLines.filterNot(_.isEmpty).map(_.trim) mkString "\n" finally source.close()
    val json = lines.parseJson
    val testCasesOpt : Seq[Option[CoreTestCase]] = json.convertTo[Seq[Option[CoreTestCase]]]
    val testCases = Seq(testCasesOpt.flatten.head)


    for {
      testCase <- testCases
    } yield {
      logger.info("Running test case: ")
      logger.info("ScriptSig: " + testCase.scriptSig)
      logger.info("ScriptPubKey: " + testCase.scriptPubKey)
      logger.info("Flags: " + testCase.flags)
      logger.info("Comments: " + testCase.comments)
      withClue(testCase.comments) {
        ScriptInterpreter.run(testCase.scriptSig, testCase.scriptPubKey) must equal (true)
      }
    }

  }








}
