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
import scala.util.Try
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
          |        "304402200929d11561cd958460371200f82e9cae64c727a495715a31828e27a7ad57b36d0220361732ced04a6f97351ecca21a56d0b8cd4932c1da1f8f569a2b68e5e48aed7801",
          |        "0479be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8",
          |        0.00000001
          |    ],
          |    "0x16 0x001491b24bf9f5288532960ac687abb035127b1d28a5",
          |    "HASH160 0x14 0x17743beb429c55c942d2ec703b98c4d57c2df5c6 EQUAL",
          |    "P2SH,WITNESS",
          |    "OK",
          |    "Basic P2SH(P2WPKH)"
          |]]
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

        val runAttemptT = Try(ScriptInterpreter.run(program))

        if (runAttemptT.isFailure) {
          throw runAttemptT.failed.get
        } else {
          runAttemptT.get must equal(testCase.expectedResult)
        }

      }
    }
  }
}
