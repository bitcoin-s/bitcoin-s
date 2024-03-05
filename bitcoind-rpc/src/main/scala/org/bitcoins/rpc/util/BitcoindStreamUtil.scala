package org.bitcoins.rpc.util

import org.apache.pekko.NotUsed
import org.apache.pekko.stream.scaladsl.Flow
import org.bitcoins.commons.jsonmodels.bitcoind.GetBlockHeaderResult
import org.bitcoins.core.protocol.blockchain.Block
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.rpc.client.common.BitcoindRpcClient

import scala.concurrent.ExecutionContext

object BitcoindStreamUtil {

  /** Creates a flow that you can feed block hashes too and the block header and block will get emitted downstream */
  def fetchBlocksBitcoind(
      bitcoindRpcClient: BitcoindRpcClient,
      parallelism: Int)(implicit ec: ExecutionContext): Flow[
    DoubleSha256DigestBE,
    (Block, GetBlockHeaderResult),
    NotUsed] = {
    Flow[DoubleSha256DigestBE].mapAsync(parallelism = parallelism) { hash =>
      val blockF = bitcoindRpcClient.getBlockRaw(hash)
      val blockHeaderResultF = bitcoindRpcClient.getBlockHeader(hash)
      for {
        block <- blockF
        blockHeaderResult <- blockHeaderResultF
      } yield (block, blockHeaderResult)
    }
  }
}
