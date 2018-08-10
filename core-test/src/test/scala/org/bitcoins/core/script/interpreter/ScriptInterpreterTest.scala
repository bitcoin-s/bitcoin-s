package org.bitcoins.core.script.interpreter

import org.bitcoins.core.crypto.{ BaseTxSigComponent, WitnessTxSigComponentP2SH, WitnessTxSigComponentRaw }
import org.bitcoins.core.currency.CurrencyUnits
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction.{ Transaction, TransactionOutput, WitnessTransaction }
import org.bitcoins.core.script.{ PreExecutionScriptProgram, ScriptProgram }
import org.bitcoins.core.script.flag.ScriptFlagFactory
import org.bitcoins.core.script.interpreter.testprotocol.CoreTestCase
import org.bitcoins.core.script.interpreter.testprotocol.CoreTestCaseProtocol._
import org.bitcoins.core.util._
import org.scalatest.{ FlatSpec, MustMatchers }
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
          | [["", "DEPTH 0 EQUAL", "P2SH,STRICTENC", "OK", "Test the test: we should have an empty stack after scriptSig evaluation"]]
   """.stripMargin*/
    val lines = try source.getLines.filterNot(_.isEmpty).map(_.trim) mkString "\n" finally source.close()
    val json = lines.parseJson
    val testCasesOpt: Seq[Option[CoreTestCase]] = json.convertTo[Seq[Option[CoreTestCase]]]
    val testCases: Seq[CoreTestCase] = testCasesOpt.flatten
    for {
      testCase <- testCases
      (creditingTx, outputIndex) = TransactionTestUtil.buildCreditingTransaction(testCase.scriptPubKey, testCase.witness.map(_._2))
      (tx, inputIndex) = TransactionTestUtil.buildSpendingTransaction(creditingTx, testCase.scriptSig, outputIndex, testCase.witness)
    } yield {
      val scriptPubKey = ScriptPubKey.fromAsm(testCase.scriptPubKey.asm)
      val flags = ScriptFlagFactory.fromList(testCase.flags)
      val witness = testCase.witness
      val txSigComponent = witness match {
        case Some((w, amount)) => scriptPubKey match {
          case p2sh: P2SHScriptPubKey =>
            val output = TransactionOutput(amount, p2sh)
            WitnessTxSigComponentP2SH(tx.asInstanceOf[WitnessTransaction], inputIndex, output, flags)

          case wit: WitnessScriptPubKey =>
            val output = TransactionOutput(amount, wit)
            val t = WitnessTxSigComponentRaw(
              transaction = tx.asInstanceOf[WitnessTransaction],
              inputIndex = inputIndex,
              output = output,
              flags = flags)
            t
          case x @ (_: P2PKScriptPubKey | _: P2PKHScriptPubKey | _: MultiSignatureScriptPubKey | _: CLTVScriptPubKey | _: CSVScriptPubKey
            | _: CLTVScriptPubKey | _: EscrowTimeoutScriptPubKey | _: NonStandardScriptPubKey | _: WitnessCommitment | EmptyScriptPubKey) =>
            val output = TransactionOutput(amount, x)
            BaseTxSigComponent(
              transaction = tx,
              inputIndex = inputIndex,
              output = output,
              flags = flags)
        }
        case None =>
          //value in the output does not matter here since it isn't covered by the digital signature
          val output = TransactionOutput(CurrencyUnits.zero, scriptPubKey)
          BaseTxSigComponent(
            transaction = tx,
            inputIndex = inputIndex,
            output = output,
            flags = flags)
      }
      val program = PreExecutionScriptProgram(txSigComponent)
      withClue(testCase.raw) {
        ScriptInterpreter.run(program) must equal(testCase.expectedResult)
      }
    }
  }
}
