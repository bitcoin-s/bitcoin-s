package org.bitcoins.server.grpc

import org.bitcoins.core.config.RegTest
import org.bitcoins.testkit.chain.MockChainApi
import org.bitcoins.testkit.fixtures.ServerGrpcFixture
import org.bitcoins.testkitcore.chain.ChainTestUtil
import org.scalatest.FutureOutcome

class ChainGrpcRoutesTest extends ServerGrpcFixture {
  override type GrpcClient = ChainRoutesClient

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    withChainRoutesClient(test)
  }

  behavior of "ChainGrpcRoutes"

  it must "getinfo" in { case clientServer =>
    val client = clientServer.client
    client.getInfo(GetInfoRequest()).map { response =>
      assert(response.network == RegTest.name)
      assert(
        response.blockHeight == ChainTestUtil.regTestGenesisHeaderDb.height)
      assert(
        response.blockHash == ChainTestUtil.regTestGenesisHeaderDb.hashBE.hex)
      assert(response.torStarted)
      assert(!response.syncing)
      assert(!response.isInitialBlockDownload)
    }
  }

  it must "getblockcount" in { case clientServer =>
    val client = clientServer.client
    client.getBlockCount(GetBlockCountRequest()).map { response =>
      assert(response.count == 0)
    }
  }

  it must "getfiltercount" in { case clientServer =>
    val client = clientServer.client
    client.getFilterCount(GetFilterCountRequest()).map { response =>
      assert(response.count == 0)
    }
  }

  it must "getfilterheadercount" in { case clientServer =>
    val client = clientServer.client
    client.getFilterHeaderCount(GetFilterHeaderCountRequest()).map { response =>
      assert(response.count == 0)
    }
  }

  it must "getbestblockhash" in { case clientServer =>
    val client = clientServer.client
    for {
      response <- client.getBestBlockHash(GetBestBlockHashRequest())
      expectedHash <- MockChainApi.getBestBlockHash()
    } yield {
      assert(response.hash == expectedHash.hex)
    }
  }

  it must "getblockheader" in { case clientServer =>
    val client = clientServer.client
    val hash = ChainTestUtil.regTestGenesisHeaderDb.hashBE.hex
    client.getBlockHeader(GetBlockHeaderRequest(hash = hash)).map { response =>
      assert(response.header.isEmpty)
    }
  }

  it must "getmediantimepast" in { case clientServer =>
    val client = clientServer.client
    client.getMedianTimePast(GetMedianTimePastRequest()).map { response =>
      assert(response.mediantimepast == 0L)
    }
  }
}
