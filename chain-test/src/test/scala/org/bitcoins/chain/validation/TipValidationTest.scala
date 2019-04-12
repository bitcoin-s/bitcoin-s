package org.bitcoins.chain.validation

import akka.actor.ActorSystem
import org.bitcoins.chain.db.ChainDbManagement
import org.bitcoins.chain.models.{
  BlockHeaderDAO,
  BlockHeaderDb,
  BlockHeaderDbHelper
}
import org.bitcoins.chain.util.ChainUnitTest
import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.bitcoins.testkit.chain.{BlockHeaderHelper, ChainTestUtil}
import org.scalatest.{Assertion, FutureOutcome}

import scala.concurrent.Future

class TipValidationTest extends ChainUnitTest {

  override type FixtureParam = BlockHeaderDAO

  override def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withBlockHeaderDAO(test)

  override implicit val system: ActorSystem = ActorSystem("TipValidationTest")

  behavior of "TipValidation"

  //blocks 566,092 and 566,093
  val newValidTip = BlockHeaderHelper.header1
  val currentTipDb = BlockHeaderHelper.header2Db
  override val chainParam = ChainTestUtil.mainnetChainParam

  it must "connect two blocks with that are valid" in { bhDAO =>
    val newValidTipDb =
      BlockHeaderDbHelper.fromBlockHeader(566093, newValidTip)
    val expected = TipUpdateResult.Success(newValidTipDb)

    runTest(newValidTip, expected, bhDAO)
  }

  it must "connect two blocks with different POW requirements at the correct interval (2016 blocks for BTC)" ignore {
    _ =>
      //https://blockstream.info/block/0000000000000000002296c06935b34f3ed946d98781ff471a99101796e8611b
      val currentTip = BlockHeader.fromHex(
        "000000202164d8c4e5246ab003fdebe36c697b9418aa454ec4190d00000000000000000059134ad5aaad38a0e75946c7d4cb09b3ad45b459070195dd564cde193cf0ef29c33e855c505b2e17f61af734")

      val currentTipDb = BlockHeaderDbHelper.fromBlockHeader(566495, currentTip)

      //https://blockstream.info/block/00000000000000000015fea169c62eb0a1161aba36932ca32bc3785cbb3480bf
      val newTip = BlockHeader.fromHex(
        "000000201b61e8961710991a47ff8187d946d93e4fb33569c09622000000000000000000d0098658f53531e6e67fc9448986b5a8f994da42d746079eabe10f55e561e243103f855c17612e1735c4afdb")

      val newTipDb = BlockHeaderDbHelper.fromBlockHeader(566496, newTip)
      val expected = TipUpdateResult.Success(newTipDb)

      runTest(newTip, expected, ???, currentTipDbDefault = currentTipDb)
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
    val checkTipF = TipValidation.checkNewTip(header,
                                              currentTipDbDefault,
                                              blockHeaderDAO,
                                              chainParam)

    checkTipF.map(validationResult => assert(validationResult == expected))
  }
}
