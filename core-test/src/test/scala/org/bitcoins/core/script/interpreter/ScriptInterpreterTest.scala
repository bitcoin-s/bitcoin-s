package org.bitcoins.core.script.interpreter

import org.bitcoins.core.crypto.{
  BaseTxSigComponent,
  WitnessTxSigComponentP2SH,
  WitnessTxSigComponentRaw
}
import org.bitcoins.core.currency.CurrencyUnits
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction.{
  Transaction,
  TransactionOutput,
  WitnessTransaction
}
import org.bitcoins.core.script.PreExecutionScriptProgram
import org.bitcoins.core.script.flag.ScriptFlagFactory
import org.bitcoins.core.script.interpreter.testprotocol.CoreTestCase
import org.bitcoins.core.script.interpreter.testprotocol.CoreTestCaseProtocol._
import org.bitcoins.testkit.util.{BitcoinSUnitTest, TransactionTestUtil}
import spray.json._

import scala.io.Source
import scala.util.Try

/**
  * Created by chris on 1/6/16.
  */
class ScriptInterpreterTest extends BitcoinSUnitTest {

  "ScriptInterpreter" must "evaluate all the scripts from the bitcoin core script_tests.json" in {

    val source = Source.fromURL(getClass.getResource("/script_tests.json"))

    //use this to represent a single test case from script_valid.json
    /*        val lines =
      """
          | [["0x01 0x80", "DUP BOOLOR", "P2SH,STRICTENC", "EVAL_FALSE", "negative-0 negative-0 BOOLOR"]]
   """.stripMargin*/
    val lines =
      try source.getLines.filterNot(_.isEmpty).map(_.trim) mkString "\n"
      finally source.close()
    val json = lines.parseJson
    val testCasesOpt: Seq[Option[CoreTestCase]] =
      json.convertTo[Seq[Option[CoreTestCase]]]
    val testCases: Seq[CoreTestCase] = testCasesOpt.flatten
    for {
      testCase <- testCases
      (creditingTx, outputIndex) =
        TransactionTestUtil
          .buildCreditingTransaction(testCase.scriptPubKey,
                                     testCase.witness.map(_._2))
      (tx, inputIndex) =
        TransactionTestUtil.buildSpendingTransaction(creditingTx,
                                                     testCase.scriptSig,
                                                     outputIndex,
                                                     testCase.witness)
    } yield {
      val scriptPubKey = ScriptPubKey.fromAsm(testCase.scriptPubKey.asm)
      val flags = ScriptFlagFactory.fromList(testCase.flags)
      val witness = testCase.witness
      val txSigComponent = witness match {
        case Some((w, amount)) =>
          scriptPubKey match {
            case p2sh: P2SHScriptPubKey =>
              val output = TransactionOutput(amount, p2sh)
              WitnessTxSigComponentP2SH(tx.asInstanceOf[WitnessTransaction],
                                        inputIndex,
                                        output,
                                        flags)

            case wit: WitnessScriptPubKey =>
              val output = TransactionOutput(amount, wit)
              val t =
                WitnessTxSigComponentRaw(transaction =
                                           tx.asInstanceOf[WitnessTransaction],
                                         inputIndex = inputIndex,
                                         output = output,
                                         flags = flags)
              t
            case x @ (_: P2PKScriptPubKey | _: P2PKHScriptPubKey |
                _: P2PKWithTimeoutScriptPubKey | _: MultiSignatureScriptPubKey |
                _: CLTVScriptPubKey | _: CSVScriptPubKey | _: CLTVScriptPubKey |
                _: ConditionalScriptPubKey | _: NonStandardScriptPubKey |
                _: WitnessCommitment | EmptyScriptPubKey) =>
              val output = TransactionOutput(amount, x)
              BaseTxSigComponent(transaction = tx,
                                 inputIndex = inputIndex,
                                 output = output,
                                 flags = flags)
          }
        case None =>
          //value in the output does not matter here since it isn't covered by the digital signature
          val output = TransactionOutput(CurrencyUnits.zero, scriptPubKey)
          BaseTxSigComponent(transaction = tx,
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

  it must "evaluate a p2sh witness transaction as valid" in {
    val p2shWitTx = Transaction(
      "02000000000101c29e1b0bc7d7eef558cab27731e68526c94a8ee30b5a8795674b39b25430dacd0000000017160014471b19074d141d11042cba016cd60d89caee0150feffffff02ee2916000000000017a91401d95721e723fb9d02fa7ae0d5a330925fa6f90187f0e864da0100000017a9149829ca82bf4d71daeffb14a6b96da4e621cccb31870247304402205406cf034993af5fc0dd9fcd96eafe04acedeb9d4d2a012aa539561cd50b6ce30220680079485226ee184c678da475e291e81ca8b83b5b973d0a8513a5b4a7e24c52012103c6350aad80873966c5a778bc3e019eef399d242ef386e21267652ca1f3e3a6ae087a1900")
    val prevOut = TransactionOutput(
      "48677bda0100000017a914885437151ad21f21c5c714ddffdf67e56fcd63ea87")

    ScriptInterpreter.verifyTransaction(p2shWitTx, Vector(prevOut))
  }

  it must "evaluate a witness transaction as valid" in {
    val wtx = Transaction(
      "02000000000101b62a936a1389f6f28a71f88be2703800e267bd0501e4fcd7c803a2eb2ca9d96f0000000000feffffff011027000000000000160014bb7e25b0e93a3d378b813e97d5d61efcfa69863e040047304402204e0401cf6bd54c3551a7534f9c04e30701da735693670028a5ccd5c99934df1a02200249b701467f461015bf6643bed692f911b6a43a05f1e18301e61ace743ba788014730440220385f39b54755df27ddc353b24dd83fd236e6ba0c39fefe2a41d2f0145498612b022036d0992f3245d4da79e0cc4562b8b40fb5325d8bc794cfa92d2bd36263fedcef014752210325815e1ecf3cdc683c16be773798b07ce3ca2af6969bb03975c5e0e0c06f7f0521031d8bcc3a6a7e93d904a89c20104f44b0bf6c157b6e761e20f2751381e8ba382452ae1f6d1900")
    val prevOut = TransactionOutput(
      "b82a0000000000002200202d501d0b2df21825d3dca7dc2bdaeea2c484c552b4ccbf687ffa887ae47e42c2")

    ScriptInterpreter.verifyTransaction(wtx, Vector(prevOut))
  }

  it must "evaluate a base transaction as valid" in {
    val tx = Transaction(
      "01000000010289c14b991dde643aca30b9010c9b37b8347f3984fa5c31e78adceeea4eefab000000006a47304402206d1ae877e2339ed74a1b203aacdb8e4dc6202f873a228d681d3075f7cd8ef95c022067383abdf0bc1839db0c26a0d77df097d815b4ef086da3872b4a928468b667f30121032fb875942a07a7606933e383c85d688b5fd3482c29035d4289638987582c3f2bffffffff02500e40a10b0000001976a9142301071cf4f2550a705c6348f33e6b33544b768488ac2752fb02000000001976a914741423cca440e7dc81d3468b832433e4db3c924288ac00000000")
    val prevOut = TransactionOutput(
      "37733ba40b0000001976a914741423cca440e7dc81d3468b832433e4db3c924288ac")

    ScriptInterpreter.verifyTransaction(tx, Vector(prevOut))
  }
}
