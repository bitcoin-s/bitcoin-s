package org.bitcoins.chain.validation

import org.bitcoins.chain.blockchain.Blockchain
import org.bitcoins.chain.blockchain.ConnectTipResult.{
  BadTip,
  ExtendChain,
  Reorg
}
import org.bitcoins.chain.pow.Pow
import org.bitcoins.commons.serializers.SerializerUtil
import org.bitcoins.core.api.chain.db.{BlockHeaderDb, BlockHeaderDbHelper}
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.blockchain.{
  Block,
  BlockHeader,
  MainNetChainParams
}
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionOutput}
import org.bitcoins.core.script.util.PreviousOutputMap
import org.bitcoins.testkitcore.chain.ChainTestUtil
import org.bitcoins.testkitcore.util.BitcoinSJvmTest
import play.api.libs.json.{JsResult, JsValue, Json, Reads}

import scala.io.Source
import scala.util.Using

class Bip54Test extends BitcoinSJvmTest {

  behavior of "BIP54 sigops"
  import org.bitcoins.commons.serializers.JsonSerializers.{
    transactionReads,
    blockReads,
    blockHeaderReads
  }
  implicit object TransactionOutputReads extends Reads[TransactionOutput] {
    override def reads(json: JsValue): JsResult[TransactionOutput] =
      SerializerUtil.processJsString(TransactionOutput.fromHex)(json)
  }
  sealed trait Bip54TestCase {
    def valid: Boolean
    def comment: String
  }
  case class SigOpsTestCase(
      spent_outputs: Vector[TransactionOutput],
      tx: Transaction,
      valid: Boolean,
      comment: String)
      extends Bip54TestCase {
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
      extends Bip54TestCase
  implicit val txSizeTestCasereader: Reads[TxSizeTestCase] =
    Json.reads[TxSizeTestCase]

  case class CoinbaseTestCase(
      block_chain: Vector[Block],
      valid: Boolean,
      comment: String)
      extends Bip54TestCase {
    val blockHeaderDbs: Vector[BlockHeaderDb] = {
      block_chain.zipWithIndex.map { case (block, height) =>
        BlockHeaderDbHelper.fromBlockHeader(
          height = height,
          chainWork = Pow.getBlockProof(block.blockHeader),
          bh = block.blockHeader
        )
      }
    }
  }
  implicit val coinbaseTestCasereader: Reads[CoinbaseTestCase] =
    Json.reads[CoinbaseTestCase]

  case class TimestampTestCase(
      header_chain: Vector[BlockHeader],
      valid: Boolean,
      comment: String)
      extends Bip54TestCase
  implicit val timestampTestCasereader: Reads[TimestampTestCase] =
    Json.reads[TimestampTestCase]

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
        // val height = testCase.block_chain.length - 1
        val genesisChain =
          Blockchain.fromHeaders(Vector(ChainTestUtil.mainnetGenesisHeaderDb))
        if (testCase.valid) {
          handleValidCoinbaseTests(testCase, genesisChain)
          succeed
        } else {
          val isValid = handleInvalidCoinbaseTests(testCase, genesisChain)
          assert(!isValid)
        }
      }
    }
    succeed
  }

  it must "pass all timestamp test vectors" in {
    val fileName =
      "/timestamps.json"
    val lines = Using(Source.fromURL(getClass.getResource(fileName))) {
      source => source.mkString
    }.get
    val json = Json.parse(lines)
    val testCases = json.validate[Vector[TimestampTestCase]].get
    testCases.foreach { testCase =>
      withClue(testCase.comment) {
        val genesisChain =
          Blockchain.fromHeaders(Vector(ChainTestUtil.mainnetGenesisHeaderDb))
        if (testCase.valid) {
          handleValidTimestampTests(testCase, genesisChain)
          ()
        } else {
          val isValid = handleInvalidTimestampTests(testCase, genesisChain)
          assert(!isValid)
          ()
        }
      }
    }
    succeed
  }

  private def handleValidCoinbaseTests(
      testCase: CoinbaseTestCase,
      genesisChain: Blockchain): Unit = {
    testCase.block_chain.tail.foldLeft(genesisChain) { case (chain, block) =>
      val blockCheck =
        TipValidation.contextualCheckBlock(block = block, blockchain = chain)
      assert(blockCheck)
      val connTipResult = Blockchain.connectTip(header = block.blockHeader,
                                                blockchain = chain,
                                                chainParams =
                                                  MainNetChainParams)
      connTipResult match {
        case _: BadTip | _: Reorg => fail(s"Could not connect block: $block")
        case ExtendChain(_, newChain) => newChain
      }
    }
  }

  private def handleInvalidCoinbaseTests(
      testCase: CoinbaseTestCase,
      genesisChain: Blockchain): Boolean = {
    testCase.block_chain.tail
      .foldLeft((genesisChain, true)) { case ((chain, valid), block) =>
        if (valid) {
          // means previous block was valid, so check this one
          val blockCheck =
            TipValidation.contextualCheckBlock(block = block,
                                               blockchain = chain)
          val connTipResult = Blockchain.connectTip(header = block.blockHeader,
                                                    blockchain = chain,
                                                    chainParams =
                                                      MainNetChainParams)
          connTipResult match {
            case _: BadTip | _: Reorg =>
              fail(s"Could not connect block: $block")
            case ExtendChain(_, newChain) => (newChain, blockCheck)
          }
        } else {
          (chain, valid)
        }
      }
      ._2
  }

  private def handleValidTimestampTests(
      testCase: TimestampTestCase,
      genesisChain: Blockchain): Unit = {
    testCase.header_chain.tail.foldLeft(genesisChain) { case (chain, header) =>
      val headerCheck =
        TipValidation.contextualCheckBlockHeader(header = header,
                                                 blockchain = chain,
                                                 chainParams =
                                                   MainNetChainParams)
      assert(headerCheck)
      val connTipResult = Blockchain.connectTip(header = header,
                                                blockchain = chain,
                                                chainParams =
                                                  MainNetChainParams)
      connTipResult match {
        case _: BadTip | _: Reorg => fail(s"Could not connect header: $header")
        case ExtendChain(_, newChain) => newChain
      }
    }
  }

  private def handleInvalidTimestampTests(
      testCase: TimestampTestCase,
      genesisChain: Blockchain): Boolean = {
    testCase.header_chain.tail
      .foldLeft((genesisChain, true)) { case ((chain, valid), header) =>
        if (valid) {
          // means previous header was valid, so check this one
          val headerCheck =
            TipValidation.contextualCheckBlockHeader(header = header,
                                                     blockchain = chain,
                                                     chainParams =
                                                       MainNetChainParams)
          val connTipResult = Blockchain.connectTip(header = header,
                                                    blockchain = chain,
                                                    chainParams =
                                                      MainNetChainParams)
          connTipResult match {
            case _: BadTip | _: Reorg =>
              fail(s"Could not connect header: $header")
            case ExtendChain(_, newChain) => (newChain, headerCheck)
          }
        } else {
          (chain, valid)
        }
      }
      ._2
  }
}
