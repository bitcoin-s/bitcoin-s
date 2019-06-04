package org.bitcoins.testkit.chain

import org.bitcoins.chain.models.{BlockHeaderDb, BlockHeaderDbHelper}
import org.bitcoins.core.crypto
import org.bitcoins.core.crypto.DoubleSha256DigestBE
import org.bitcoins.core.protocol.blockchain.{
  BlockHeader,
  MainNetChainParams,
  RegTestNetChainParams
}
import org.bitcoins.rpc.client.common.BitcoindRpcClient

import scala.concurrent.{ExecutionContext, Future}

sealed abstract class ChainTestUtil {
  lazy val regTestChainParams: RegTestNetChainParams.type =
    RegTestNetChainParams
  lazy val regTestHeader: BlockHeader =
    regTestChainParams.genesisBlock.blockHeader
  lazy val regTestGenesisHeaderDb: BlockHeaderDb = {
    BlockHeaderDbHelper.fromBlockHeader(height = 0, bh = regTestHeader)
  }

  lazy val mainnetChainParam: MainNetChainParams.type = MainNetChainParams

  lazy val blockHeader562375 = BlockHeader.fromHex(
    "0100000000000000000000000000000000000000000000000000000000000000000000003ba3edfd7a7b12b27ac72c3e67768f617fc81bc3888a51323a9fb8aa4b1e5e4a29ab5f49ffff001d1dac2b7c")

  lazy val blockHeader562462 = BlockHeader.fromHex(
    "0100000053fb045b4d3ca149faca8e7ea53cdb3168bc58b875e47196b3a6b3f100000000406468307c915485a9c9eabe31cc853e68311176e07e71475c3e26888fb7b7ed30846949ffff001d2b740f74")

  lazy val blockHeader562463 = BlockHeader.fromHex(
    "010000003ce6c27ae14022e4b6ea0a5c3633d156e3e3a47509c1adf085371ba300000000f01258747019514aa5c475cddd59a309347280ead98d19d8df8f9f99eb56757938866949ffff001d18bcb4f8")

  lazy val blockHeader562464 = BlockHeader.fromHex(
    "010000004bd0b78e90c6b0f361f395535ac170980de0c8214380daefce31fd1100000000282c9db8313817b4835efab229872eae2b8b5011c2e90ed14e57192984da062359896949ffff001d15c6aed8")

  /** Contains block headers where a proof of work change is valid */
  object ValidPOWChange {

    //this is the first block in the 2016 block proof of work difficulty change interval
    //https://blockstream.info/block/0000000000000000002567dc317da20ddb0d7ef922fe1f9c2375671654f9006c
    lazy val blockHeader564480 = BlockHeader.fromHex(
      "000000200cd536b3eb1cd9c028e081f1455006276b293467c3e5170000000000000000007bc1b27489db01c85d38a4bc6d2280611e9804f506d83ad00d2a33ebd663992f76c7725c505b2e174fb90f55")

    lazy val blockHeaderDb564480 =
      BlockHeaderDbHelper.fromBlockHeader(564480, blockHeader564480)

    lazy val blockHeader566494 = BlockHeader.fromHex(
      "00000020ea2cb07d670ddb7a158e72ddfcfd9e1b9bf4459278bb240000000000000000004fb33054d79de69bb84b4d5c7dd87d80473c416320427a882c72108f7e43fd0c3d3e855c505b2e178f328fe2")

    lazy val blockHeaderDb566494 =
      BlockHeaderDbHelper.fromBlockHeader(566594, blockHeader566494)

    lazy val blockHeader566495 = BlockHeader.fromHex(
      "000000202164d8c4e5246ab003fdebe36c697b9418aa454ec4190d00000000000000000059134ad5aaad38a0e75946c7d4cb09b3ad45b459070195dd564cde193cf0ef29c33e855c505b2e17f61af734")

    lazy val blockHeaderDb566495 =
      BlockHeaderDbHelper.fromBlockHeader(566495, blockHeader566495)

    //https://blockstream.info/block/00000000000000000015fea169c62eb0a1161aba36932ca32bc3785cbb3480bf
    lazy val blockHeader566496 = BlockHeader.fromHex(
      "000000201b61e8961710991a47ff8187d946d93e4fb33569c09622000000000000000000d0098658f53531e6e67fc9448986b5a8f994da42d746079eabe10f55e561e243103f855c17612e1735c4afdb")

    lazy val blockHeaderDb566496 =
      BlockHeaderDbHelper.fromBlockHeader(566496, blockHeader566496)
  }

  /** Creates a best block header function for [[org.bitcoins.chain.blockchain.sync.ChainSync.sync() ChainSync.sync]] */
  def bestBlockHashFnRpc(bitcoindF: Future[BitcoindRpcClient])(
      implicit ec: ExecutionContext): () => Future[DoubleSha256DigestBE] = {
    () =>
      bitcoindF.flatMap(_.getBestBlockHash)
  }

  /** Creates a getBlocKHeader function for [[org.bitcoins.chain.blockchain.sync.ChainSync.sync() ChainSync.sync]] */
  def getBlockHeaderFnRpc(bitcoindF: Future[BitcoindRpcClient])(
      implicit ec: ExecutionContext): DoubleSha256DigestBE => Future[
    BlockHeader] = { hash: crypto.DoubleSha256DigestBE =>
    bitcoindF.flatMap(_.getBlockHeader(hash).map(_.blockHeader))
  }
}

object ChainTestUtil extends ChainTestUtil
