package org.bitcoins.core.script.interpreter


import org.bitcoins.core.crypto.{ECPrivateKey, TransactionSignatureComponent, TransactionSignatureSerializer}
import org.bitcoins.core.currency.CurrencyUnits
import org.bitcoins.core.gen.TransactionGenerators
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction.WitnessTransaction
import org.bitcoins.core.script.ScriptProgram
import org.bitcoins.core.script.flag.ScriptFlagFactory
import org.bitcoins.core.script.interpreter.testprotocol.CoreTestCaseProtocol._
import org.bitcoins.core.script.interpreter.testprotocol.{CoreTestCase, CoreTestCaseProtocol}
import org.bitcoins.core.script.result.ScriptErrorUnsatisfiedLocktime
import org.bitcoins.core.util._
import org.scalatest.{FlatSpec, MustMatchers}
import spray.json._

import scala.io.Source
/**
 * Created by chris on 1/6/16.
 */
class ScriptInterpreterTest extends FlatSpec with MustMatchers with ScriptInterpreter with BitcoinSLogger {

  "ScriptInterpreter" must "evaluate all the scripts from the bitcoin core script_tests.json" in {

    val source = Source.fromURL(getClass.getResource("/script_tests.json"))


    //use this to represent a single test case from script_valid.json
/*    val lines =
        """
          | [ [
          |  [
          |   "3044022066e02c19a513049d49349cf5311a1b012b7c4fae023795a18ab1d91c23496c22022025e216342c8e07ce8ef51e8daee88f84306a9de66236cab230bb63067ded1ad301",
          |   "410479be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8ac",
          |   0.00000001
          |  ],
          |  "0x22 0x0020b95237b48faaa69eb078e1170be3b5cbb3fddf16d0a991e14ad274f7b33a4f64",
          |  "HASH160 0x14 0xf386c2ba255cc56d20cfa6ea8b062f8b59945518 EQUAL",
          |  "P2SH,WITNESS",
          |  "OK",
          |  "Basic P2SH(P2WSH)"
          | ]]
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
        case Some((w, amount)) => ScriptProgram(tx.asInstanceOf[WitnessTransaction], scriptPubKey, inputIndex, flags, w, amount)
        case None => ScriptProgram(tx, scriptPubKey, inputIndex, flags)
      }
      //tx.hex must be ("0100000000010190491d88b9f0dc24d271f0f67179bce5914afe1ac0f83f6cd205f8b807436d6f0000000000ffffffff010100000000000000000247304402200d461c140cfdfcf36b94961db57ae8c18d1cb80e9d95a9e47ac22470c1bf125502201c8dc1cbfef6a3ef90acbbb992ca22fe9466ee6f9d4898eda277a7ac3ab4b2510143410479be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8ac00000000")

      withClue(testCase.raw) {
        ScriptInterpreter.run(program) must equal (testCase.expectedResult)
      }
    }

    //"01000000529eb5e7c5f1f2816e901cd5d18f9864e91b315c578b351caf3f0cf595d5547e3bb13029ce7b1f559ef5e747fcac439f1455a2ec7c5f09b72290795e7066504401eeb685e31e8e0f0277d48c715a912bda8df2a15762b9e5fbed45c964078c0f00000000220020b95237b48faaa69eb078e1170be3b5cbb3fddf16d0a991e14ad274f7b33a4f640100000000000000ffffffffe5d196bfb21caca9dbd654cafb3b4dc0c4882c8927d2eb300d9539dd0b9342280000000001000000" must be ("01000000529eb5e7c5f1f2816e901cd5d18f9864e91b315c578b351caf3f0cf595d5547e3bb13029ce7b1f559ef5e747fcac439f1455a2ec7c5f09b72290795e7066504401eeb685e31e8e0f0277d48c715a912bda8df2a15762b9e5fbed45c964078c0f0000000043410479be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8ac0100000000000000ffffffffe5d196bfb21caca9dbd654cafb3b4dc0c4882c8927d2eb300d9539dd0b9342280000000001000000")
  }
}
