package org.bitcoins.chain.validation

import akka.actor.ActorSystem
import org.bitcoins.chain.db.ChainDbManagement
import org.bitcoins.chain.models.{
  BlockHeaderDAO,
  BlockHeaderDb,
  BlockHeaderDbHelper
}
import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.bitcoins.testkit.chain.{
  BlockHeaderHelper,
  ChainTestUtil,
  ChainUnitTest
}
import org.scalatest.{Assertion, FutureOutcome}

import scala.concurrent.Future
import org.bitcoins.chain.config.ChainAppConfig
import com.typesafe.config.ConfigFactory
import org.bitcoins.server.BitcoinSAppConfig

class TipValidationTest extends ChainUnitTest {

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

  it must "connect two blocks with that are valid" in { bhDAO =>
    val newValidTipDb =
      BlockHeaderDbHelper.fromBlockHeader(566093, newValidTip)
    val expected = TipUpdateResult.Success(newValidTipDb)

    runTest(newValidTip, expected, bhDAO)
  }

  it must "fail to connect two blocks that do not reference prev block hash correctly" in {
    bhDAO =>
      val badPrevHash = BlockHeaderHelper.badPrevHash

      val expected = TipUpdateResult.BadPreviousBlockHash(badPrevHash)

      runTest(badPrevHash, expected, bhDAO)
  }

  it must "fail to connect two blocks with two different POW requirements at the wrong interval" in {
    bhDAO =>
      val badPOW = BlockHeaderHelper.badNBits
      val expected = TipUpdateResult.BadPOW(badPOW)
      runTest(badPOW, expected, bhDAO)
  }

  it must "fail to connect two blocks with a bad nonce" in { bhDAO =>
    val badNonce = BlockHeaderHelper.badNonce
    val expected = TipUpdateResult.BadNonce(badNonce)
    runTest(badNonce, expected, bhDAO)
  }

  private def runTest(
      header: BlockHeader,
      expected: TipUpdateResult,
      blockHeaderDAO: BlockHeaderDAO,
      currentTipDbDefault: BlockHeaderDb = currentTipDb): Future[Assertion] = {
    val checkTipF =
      TipValidation.checkNewTip(header, currentTipDbDefault, blockHeaderDAO)

    checkTipF.map(validationResult => assert(validationResult == expected))
  }
}
