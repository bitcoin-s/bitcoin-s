package org.bitcoins.core.script.interpreter


import org.bitcoins.core.crypto.BaseTxSigComponent
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction.WitnessTransaction
import org.bitcoins.core.script.ScriptProgram
import org.bitcoins.core.script.flag.ScriptFlagFactory
import org.bitcoins.core.script.interpreter.testprotocol.CoreTestCase
import org.bitcoins.core.script.interpreter.testprotocol.CoreTestCaseProtocol._
import org.bitcoins.core.util._
import org.scalatest.{FlatSpec, MustMatchers}
import spray.json._

import scala.io.Source
/**
 * Created by chris on 1/6/16.
 */
class ScriptInterpreterTest extends FlatSpec with MustMatchers {
  private def logger = BitcoinSLogger.logger
  "ScriptInterpreter" must "evaluate all the scripts from the bitcoin core script_tests.json" in {

    val source = Source.fromURL(getClass.getResource("/script_tests.json"))


    //use this to represent a single test case from script_valid.json
/*    val lines =
        """
          | [[
          |    [
          |        "304402200d461c140cfdfcf36b94961db57ae8c18d1cb80e9d95a9e47ac22470c1bf125502201c8dc1cbfef6a3ef90acbbb992ca22fe9466ee6f9d4898eda277a7ac3ab4b25101",
          |        "410479be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8ac",
          |        0.00000001
          |    ],
          |    "",
          |    "0 0x20 0xb95237b48faaa69eb078e1170be3b5cbb3fddf16d0a991e14ad274f7b33a4f64",
          |    "P2SH,WITNESS",
          |    "OK",
          |    "Basic P2WSH"
          |]]
   """.stripMargin*/
    val lines = try source.getLines.filterNot(_.isEmpty).map(_.trim) mkString "\n" finally source.close()
    val json = lines.parseJson
    val testCasesOpt : Seq[Option[CoreTestCase]] = json.convertTo[Seq[Option[CoreTestCase]]]
    val testCases : Seq[CoreTestCase] = testCasesOpt.flatten
    for {
      testCase <- testCases
      (creditingTx,outputIndex) = TransactionTestUtil.buildCreditingTransaction(testCase.scriptPubKey, testCase.witness.map(_._2))
      (tx,inputIndex) = TransactionTestUtil.buildSpendingTransaction(creditingTx,testCase.scriptSig,outputIndex, testCase.witness)
    } yield {
      logger.info("Raw test case: " + testCase.raw)
      logger.info("Parsed ScriptSig: " + testCase.scriptSig)
      logger.info("Parsed ScriptPubKey: " + testCase.scriptPubKey)
      logger.info("Parsed tx: " + tx.hex)
      logger.info("Flags: " + testCase.flags)
      logger.info("Comments: " + testCase.comments)
      val scriptPubKey = ScriptPubKey.fromAsm(testCase.scriptPubKey.asm)
      val flags = ScriptFlagFactory.fromList(testCase.flags)
      val witness = testCase.witness
      logger.info("Flags after parsing: " + flags)
      logger.info("Witness after parsing: " + witness)
      val program = witness match {
        case Some((w, amount)) => scriptPubKey match {
          case p2sh: P2SHScriptPubKey =>
            ScriptProgram(tx.asInstanceOf[WitnessTransaction], p2sh, inputIndex, flags, amount)
          case wit: WitnessScriptPubKey =>
            ScriptProgram(tx.asInstanceOf[WitnessTransaction], wit, inputIndex, flags, amount)
          case x @ (_: P2PKScriptPubKey | _: P2PKHScriptPubKey | _: MultiSignatureScriptPubKey | _: CLTVScriptPubKey | _: CSVScriptPubKey
                    | _: CLTVScriptPubKey | _: EscrowTimeoutScriptPubKey | _: NonStandardScriptPubKey | _: WitnessCommitment | EmptyScriptPubKey) =>
            val t = BaseTxSigComponent(tx,inputIndex,x,flags)
            ScriptProgram(t)
        }
        case None => ScriptProgram(tx, scriptPubKey, inputIndex, flags)
      }
      withClue(testCase.raw) {
        ScriptInterpreter.run(program) must equal (testCase.expectedResult)
      }
    }
  }
}
