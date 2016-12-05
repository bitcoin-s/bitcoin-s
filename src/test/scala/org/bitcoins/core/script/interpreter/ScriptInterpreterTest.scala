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
          | [[
          |  [
          |   "304402205ae57ae0534c05ca9981c8a6cdf353b505eaacb7375f96681a2d1a4ba6f02f84022056248e68643b7d8ce7c7d128c9f1f348bcab8be15d094ad5cadd24251a28df8001",
          |   "0479be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8",
          |   0.00000000
          |  ],
          |  "",
          |  "1 0x14 0x91b24bf9f5288532960ac687abb035127b1d28a5",
          |  "DISCOURAGE_UPGRADABLE_WITNESS_PROGRAM,P2SH,WITNESS",
          |  "DISCOURAGE_UPGRADABLE_WITNESS_PROGRAM",
          |  "P2WPKH with future witness version"
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

    //"010000002428d8b85b81c7e789120f7d2d44bad1198e3bed6224180c3323bc1ebd4a2aff3bb13029ce7b1f559ef5e747fcac439f1455a2ec7c5f09b72290795e7066504461653b188b3cb71d5c002f9102d0543d37ffa9719eec6e220d378b54e10d8d340000000016001491b24bf9f5288532960ac687abb035127b1d28a50100000000000000ffffffffe5d196bfb21caca9dbd654cafb3b4dc0c4882c8927d2eb300d9539dd0b9342280000000001000000" must be ("010000002428d8b85b81c7e789120f7d2d44bad1198e3bed6224180c3323bc1ebd4a2aff3bb13029ce7b1f559ef5e747fcac439f1455a2ec7c5f09b72290795e7066504461653b188b3cb71d5c002f9102d0543d37ffa9719eec6e220d378b54e10d8d34000000001976a91491b24bf9f5288532960ac687abb035127b1d28a588ac0100000000000000ffffffffe5d196bfb21caca9dbd654cafb3b4dc0c4882c8927d2eb300d9539dd0b9342280000000001000000")
  }
}
