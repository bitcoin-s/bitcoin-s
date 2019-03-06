package org.bitcoins.chain.validation

import org.bitcoins.chain.models.{BlockHeaderDb, BlockHeaderDbHelper}
import org.bitcoins.core.crypto.{DoubleSha256Digest, DoubleSha256DigestBE}
import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.bitcoins.testkit.chain.{BlockHeaderHelper, ChainTestUtil}
import org.bitcoins.testkit.util.BitcoinSUnitTest
import org.scalatest.Assertion

class TipValidationTest extends BitcoinSUnitTest {

  behavior of "TipValidation"

  //blocks 566,092 and 566,093
  val newValidTip = BlockHeaderHelper.header1
  val currentTipDb = BlockHeaderHelper.header2Db
  val chainParam = ChainTestUtil.mainnetChainParam

  it must "connect two blocks with that are valid" in {

    val newValidTipDb = BlockHeaderDbHelper.fromBlockHeader(566093, newValidTip)
    val expected = TipUpdateResult.Success(newValidTipDb)

    runTest(newValidTip, expected)
  }

  it must "connect two blocks with different POW requirements at the correct interval (2016 blocks for BTC)" in {
    //https://blockstream.info/block/0000000000000000002296c06935b34f3ed946d98781ff471a99101796e8611b
    val currentTip = BlockHeader.fromHex(
      "000000202164d8c4e5246ab003fdebe36c697b9418aa454ec4190d00000000000000000059134ad5aaad38a0e75946c7d4cb09b3ad45b459070195dd564cde193cf0ef29c33e855c505b2e17f61af734")

    val currentTipDb = BlockHeaderDbHelper.fromBlockHeader(566495, currentTip)

    //https://blockstream.info/block/00000000000000000015fea169c62eb0a1161aba36932ca32bc3785cbb3480bf
    val newTip = BlockHeader.fromHex(
      "000000201b61e8961710991a47ff8187d946d93e4fb33569c09622000000000000000000d0098658f53531e6e67fc9448986b5a8f994da42d746079eabe10f55e561e243103f855c17612e1735c4afdb")

    val newTipDb = BlockHeaderDbHelper.fromBlockHeader(566496, newTip)
    val expected = TipUpdateResult.Success(newTipDb)

    runTest(newTip, expected, currentTipDbDefault = currentTipDb)
  }

  it must "fail to connect two blocks that do not reference prev block hash correctly" in {

    val badPrevHash = BlockHeaderHelper.badPrevHash

    val expected = TipUpdateResult.BadPreviousBlockHash(badPrevHash)

    runTest(badPrevHash, expected)
  }

  it must "fail to connect two blocks with two different POW requirements at the wrong interval" in {
    val badPOW = BlockHeaderHelper.badNBits
    val expected = TipUpdateResult.BadPOW(badPOW)
    runTest(badPOW, expected)
  }

  it must "fail to connect two blocks with a bad nonce" in {
    val badNonce = BlockHeaderHelper.badNonce
    val expected = TipUpdateResult.BadNonce(badNonce)
    runTest(badNonce, expected)
  }

  private def runTest(
      header: BlockHeader,
      expected: TipUpdateResult,
      currentTipDbDefault: BlockHeaderDb = currentTipDb): Assertion = {
    val validationResult =
      TipValidation.checkNewTip(header, currentTipDbDefault, chainParam)

    assert(validationResult == expected)
  }
}
