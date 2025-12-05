package org.bitcoins.chain.validation

import org.bitcoins.chain.pow.Pow
import org.bitcoins.commons.serializers.SerializerUtil
import org.bitcoins.core.api.chain.db.BlockHeaderDbHelper
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.blockchain.Block
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionOutput}
import org.bitcoins.core.script.util.PreviousOutputMap
import org.bitcoins.testkitcore.util.BitcoinSJvmTest
import play.api.libs.json.{JsResult, JsValue, Json, Reads}

import scala.io.Source
import scala.util.Using

class Bip54Test extends BitcoinSJvmTest {

  behavior of "BIP54 sigops"
  import org.bitcoins.commons.serializers.JsonSerializers.{
    transactionReads,
    blockReads
  }
  implicit object TransactionOutputReads extends Reads[TransactionOutput] {
    override def reads(json: JsValue): JsResult[TransactionOutput] =
      SerializerUtil.processJsString(TransactionOutput.fromHex)(json)
  }
  case class SigOpsTestCase(
      spent_outputs: Vector[TransactionOutput],
      tx: Transaction,
      valid: Boolean,
      comment: String) {
    val outputMap: PreviousOutputMap = {
      val m = tx.inputs.map(_.previousOutput).zip(spent_outputs).toMap
      PreviousOutputMap(m)
    }
    val spendingTx: Transaction =
      Transaction.fromSpentOutputs(initTx = tx, spentOutputs = outputMap)
  }
  implicit val sigOpsTestCaseReader: Reads[SigOpsTestCase] =
    Json.reads[SigOpsTestCase]

  case class TxSizeTestCase(tx: Transaction, valid: Boolean, comment: String)
  implicit val txSizeTestCasereader: Reads[TxSizeTestCase] =
    Json.reads[TxSizeTestCase]

  case class CoinbaseTestCase(
      block_chain: Vector[Block],
      valid: Boolean,
      comment: String)
  implicit val coinbaseTestCasereader: Reads[CoinbaseTestCase] =
    Json.reads[CoinbaseTestCase]

  it must "pass all bip54 sigops test vectors" in {
    val fileName =
      "/sigops.json"
    val lines = Using(Source.fromURL(getClass.getResource(fileName))) {
      source => source.mkString
    }.get
    val json = Json.parse(lines)
    val testCases = json.validate[Vector[SigOpsTestCase]].get
    testCases.foreach { testCase =>
      withClue(testCase.comment) {
        if (testCase.valid) {
          assert(
            Policy.checkBip54SigOpLimit(testCase.spendingTx,
                                        testCase.spent_outputs))
        } else {
          assert(
            !Policy.checkBip54SigOpLimit(testCase.spendingTx,
                                         testCase.spent_outputs))
        }
      }
    }
    succeed
  }

  it must "pass all txsize test vectors" in {
    val fileName =
      "/txsize.json"
    val lines = Using(Source.fromURL(getClass.getResource(fileName))) {
      source => source.mkString
    }.get
    val json = Json.parse(lines)
    val testCases = json.validate[Vector[TxSizeTestCase]].get
    testCases.foreach { testCase =>
      withClue(testCase.comment) {
        if (testCase.valid) {
          assert(
            Policy.checkTransactionSizeLimit(testCase.tx)
          )
        } else {
          assert(
            !Policy.checkTransactionSizeLimit(testCase.tx)
          )
        }
      }
    }
    succeed
  }

  it must "pass all coinbases test vectors" in {
    val fileName =
      "/coinbases.json"
    val lines = Using(Source.fromURL(getClass.getResource(fileName))) {
      source => source.mkString
    }.get
    val json = Json.parse(lines)
    val testCases = json.validate[Vector[CoinbaseTestCase]].get
    testCases.foreach { testCase =>
      withClue(testCase.comment) {
        val height = testCase.block_chain.length - 1
        val tip = testCase.block_chain(height - 1).blockHeader
        val tipDb = BlockHeaderDbHelper.fromBlockHeader(
          height = height - 1,
          chainWork = Pow.getBlockProof(tip),
          bh = tip
        )
        if (testCase.valid) {

          assert(
            TipValidation.contextualCheckBlock(testCase.block_chain(height),
                                               tipDb)
          )
        } else {
          assert(
            !TipValidation.contextualCheckBlock(testCase.block_chain(height),
                                                tipDb)
          )
        }
      }
    }
    succeed
  }
}
