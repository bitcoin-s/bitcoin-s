package org.bitcoins.server.grpc

import org.bitcoins.core.config.RegTest
import org.bitcoins.testkit.fixtures.ServerGrpcFixture
import org.scalatest.FutureOutcome

class ChainGrpcRoutesTest extends ServerGrpcFixture {
  override type GrpcClient = ChainRoutesClient

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    withChainRoutesClient(test)
  }

  behavior of "ChainGrpcRoutes"

  it must "getinfo" in { case clientServer =>
    val client = clientServer.client

    for {
      response <- client.getInfo(GetInfoRequest())
      bitcoind <- cachedBitcoindWithFundsF
      expectedHash <- bitcoind.getBestBlockHash()
      height <- bitcoind.getBlockCount()
    } yield {
      assert(response.network == RegTest.name)
      assert(response.blockHeight == height)
      assert(response.blockHash == expectedHash.hex)
      assert(response.torStarted)
      assert(!response.syncing)
      assert(!response.isInitialBlockDownload)
    }
  }

  it must "getblockcount" in { case clientServer =>
    val client = clientServer.client
    for {
      response <- client.getBlockCount(GetBlockCountRequest())
      bitcoind <- cachedBitcoindWithFundsF
      expectedCount <- bitcoind.getBlockCount()
    } yield {
      assert(response.count == expectedCount)
    }
  }

  it must "getfiltercount" in { case clientServer =>
    val client = clientServer.client
    for {
      response <- client.getFilterCount(GetFilterCountRequest())
      bitcoind <- cachedBitcoindWithFundsF
      expectedCount <- bitcoind.getFilterCount()
    } yield {
      assert(response.count == expectedCount)
    }
  }

  it must "getfilterheadercount" in { case clientServer =>
    val client = clientServer.client
    for {
      response <- client.getFilterHeaderCount(GetFilterHeaderCountRequest())
      bitcoind <- cachedBitcoindWithFundsF
      expectedCount <- bitcoind.getFilterCount()
    } yield {
      assert(response.count == expectedCount)
    }
  }

  it must "getbestblockhash" in { case clientServer =>
    val client = clientServer.client
    for {
      response <- client.getBestBlockHash(GetBestBlockHashRequest())
      bitcoind <- cachedBitcoindWithFundsF
      expectedHash <- bitcoind.getBestBlockHash()
    } yield {
      assert(response.hash == expectedHash.hex)
    }
  }

  it must "getblockheader" in { case clientServer =>
    val client = clientServer.client
    for {
      bitcoind <- cachedBitcoindWithFundsF
      bestHash <- bitcoind.getBestBlockHash()
      expected <- bitcoind.getBlockHeader(bestHash)
      response <- client.getBlockHeader(
        GetBlockHeaderRequest(hash = bestHash.hex))
    } yield {
      assert(response.header.isDefined)
      val header = response.header.get
      assert(header.hash == expected.hash.hex)
      assert(header.confirmations == expected.confirmations)
      assert(header.height == expected.height)
      assert(header.version == expected.version)
      assert(header.versionHex == expected.versionHex.hex)
      assert(header.merkleroot == expected.merkleroot.hex)
      assert(header.time == expected.time.toInt)
      assert(header.mediantime == expected.mediantime.toInt)
      assert(header.nonce == expected.nonce.toInt)
      assert(header.bits == expected.bits.hex)
      assert(header.chainwork == expected.chainwork)
      assert(header.previousblockhash == expected.previousblockhash.map(_.hex))
      assert(header.nextblockhash == expected.nextblockhash.map(_.hex))
      assert(BigDecimal(header.difficulty) == expected.difficulty)
      assert(header.target == expected.target)
    }
  }

  it must "getmediantimepast" in { case clientServer =>
    val client = clientServer.client
    for {
      response <- client.getMedianTimePast(GetMedianTimePastRequest())
      bitcoind <- cachedBitcoindWithFundsF
      expected <- bitcoind.getMedianTimePast()
    } yield {
      assert(response.mediantimepast == expected)
    }
  }
}
