package org.bitcoins.chain.validation

import akka.actor.ActorSystem
import org.bitcoins.chain.models.BlockHeaderDAO
import org.bitcoins.chain.pow.Pow
import org.bitcoins.core.api.chain.db.BlockHeaderDbHelper
import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.bitcoins.testkit.chain.{BlockHeaderHelper, ChainDbUnitTest}
import org.scalatest.{Assertion, FutureOutcome}

class TipValidationTest extends ChainDbUnitTest {
  import org.bitcoins.chain.blockchain.Blockchain
  import org.bitcoins.chain.config.ChainAppConfig

  override type FixtureParam = BlockHeaderDAO

  // we're working with mainnet data
  implicit override lazy val appConfig: ChainAppConfig = mainnetAppConfig

  override def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withBlockHeaderDAO(test)

  implicit override val system: ActorSystem = ActorSystem("TipValidationTest")

  behavior of "TipValidation"

  //blocks 566,092 and 566,093
  val newValidTip = BlockHeaderHelper.header1
  val currentTipDb = BlockHeaderHelper.header2Db
  val blockchain = Blockchain.fromHeaders(Vector(currentTipDb))

  it must "connect two blocks with that are valid" in { _ =>
    val newValidTipDb =
      BlockHeaderDbHelper.fromBlockHeader(
        566093,
        currentTipDb.chainWork + Pow.getBlockProof(newValidTip),
        newValidTip)
    val expected = TipUpdateResult.Success(newValidTipDb)

    runTest(newValidTip, expected, blockchain)
  }

  it must "fail to connect two blocks that do not reference prev block hash correctly" in {
    bhDAO =>
      val badPrevHash = BlockHeaderHelper.badPrevHash

      val expected = TipUpdateResult.BadPreviousBlockHash(badPrevHash)

      runTest(badPrevHash, expected, blockchain)
  }

  it must "fail to connect two blocks with two different POW requirements at the wrong interval" in {
    bhDAO =>
      val badPOW = BlockHeaderHelper.badNBits
      val expected = TipUpdateResult.BadPOW(badPOW)
      runTest(badPOW, expected, blockchain)
  }

  it must "fail to connect two blocks with a bad nonce" in { bhDAO =>
    val badNonce = BlockHeaderHelper.badNonce
    val expected = TipUpdateResult.BadNonce(badNonce)
    runTest(badNonce, expected, blockchain)
  }

  private def runTest(
      header: BlockHeader,
      expected: TipUpdateResult,
      blockchain: Blockchain): Assertion = {
    val result = TipValidation.checkNewTip(header, blockchain)
    assert(result == expected)
  }
}
